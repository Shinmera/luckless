#|
 This file is a part of Luckless
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>

 A lock-free single-reader multiple-writer queue that can resize.
 
 This implementation is not based on any papers and instead
 manually devised. It is not proven correct, but appears
 correct in rigorous tests.

 Note:
 we spin in several locations, but this is preferable to locking,
 as we don't expect a lot of contention, and would vastly prefer
 the thread not be suspended at any time.
|#

(in-package #:org.shirakumo.luckless.queue)

(atomics:defstruct (queue
            (:constructor %make-queue (elements reallocating)))
  (elements NIL :type simple-vector)
  (write-index 0 :type (unsigned-byte 32))
  (allocation-index 0 :type (unsigned-byte 32))
  (read-index 0 :type (unsigned-byte 32))
  (reallocating NIL :type simple-vector))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type T :identity T)
    (format stream "READ-INDEX: ~d WRITE-INDEX: ~d CAPACITY: ~d"
            (queue-read-index queue)
            (queue-write-index queue)
            (cl:length (queue-elements queue)))))

(defun make-queue (&optional (initial-size 1024))
  (let ((elements (make-array initial-size :initial-element NIL)))
    (%make-queue elements elements)))

(defun push (element queue)
  (declare (optimize speed (safety 1)))
  (let ((write (queue-allocation-index queue)))
    ;; 1. allocate the space in the elements vector
    (loop until (cas (queue-allocation-index queue) write (1+ write))
          do (setf write (queue-allocation-index queue)))
    ;; 2. check if we actually have space in the vector
    (let ((elements (queue-elements queue)))
      (when (<= (cl:length elements) write)
        ;; 2.1 allocate a new vector and try to claim it in the back buffer
        (let ((new (make-array (* 2 (cl:length elements)) :initial-element NIL)))
          (when (cas (queue-reallocating queue) elements new)
            ;; 2.2 keep copying over until the old buffer is full
            (loop for start = 0 then index
                  for index = (queue-write-index queue)
                  do (replace new elements :start2 start :end2 index)
                  until (= (cl:length elements) index))
            ;; 2.3 publish the new queue vector
            (setf (queue-elements queue) new))
          (setf elements (queue-reallocating queue))))
      ;; 3. fill the element until it's actually public
      (loop
       (setf (aref elements write) element)
       (if (eq elements (queue-elements queue))
           (return)
           (setf elements (queue-reallocating queue))))
      ;; 4. commit the write by bumping the index
      (loop until (cas (queue-write-index queue) write (1+ write))))))

(defun discard (queue)
  (declare (optimize speed (safety 1)))
  (let ((elements (queue-elements queue))
        (read (queue-read-index queue))
        (write (queue-write-index queue)))
    ;; We simply clear out the elements with NIL. We can't set them to be tentative
    ;; as that would freeze up the read loop. Note that this will also not reset the
    ;; read or write indices, a read loop needs to happen first for those to be reset.
    ;; We may also "miss out" on elements that are written to the queue concurrently,
    ;; but that shouldn't be a problem, as we only guarantee to clear out elements
    ;; set prior to the call of QUEUE-DISCARD.
    (loop for i from read below write
          do (setf (aref elements i) NIL))
    queue))

(defun mapc (function queue)
  (declare (optimize speed (safety 1)))
  (let ((elements (queue-elements queue))
        (read (queue-read-index queue))
        (write (queue-write-index queue))
        (function (etypecase function
                    (function function)
                    (symbol (fdefinition function)))))
    (loop (cond ((<= write read)
                 ;; We reached beyond the write head. Try to reset the allocation head
                 ;; and exit processing
                 (cond ((or (< (queue-allocation-index queue) write)
                            (cas (queue-allocation-index queue) write 0))
                        (cond ((cas (queue-write-index queue) write 0)
                               (setf (queue-read-index queue) 0)
                               (return))
                              (T
                               (setf write (queue-write-index queue)))))
                       (T
                        ;; The write head changed, update it
                        (setf elements (queue-elements queue))
                        (setf write (queue-write-index queue)))))
                ((< read (cl:length elements))
                 ;; We're within the bounds, read an element
                 (let ((element (aref elements read)))
                   (cond ((null element)
                          ;; The current element has been discarded, skip it
                          (incf read))
                         (T
                          (setf (queue-read-index queue) (1+ read))
                          ;; We got a proper element, increase the read index and null the element
                          (setf (aref elements read) NIL)
                          ;; Call the function with the element
                          (funcall function element)
                          ;; Reset the read index as we might have processed more within the function
                          (setf read (queue-read-index queue))
                          (when (= 0 read) (return))))))
                (T
                 (setf elements (queue-elements queue)))))
    queue))

(defun length (queue)
  (- (queue-write-index queue)
     (queue-read-index queue)))
