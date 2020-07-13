#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>

 Based on:
   A Lock-Free Wait-Free Hash Table
   Cliff Click, Azul Systems, 2007
     https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/ConcurrentAutoTable.java
|#

(in-package #:org.shirakumo.luckless.hashtable)

(defconstant global-hash (if (boundp 'global-hash)
                             global-hash
                             #.(sxhash (get-universal-time))))

(defstruct (cat
            (:constructor %make-cat (next table))
            (:conc-name %cat-))
  (resizers 0 :type fixnum)
  (next NIL :type T)
  (sum-cache most-negative-fixnum :type fixnum)
  (fuzzy-sum-cache 0 :type fixnum)
  (fuzzy-time 0 :type fixnum)
  (table NIL :type simple-vector))

(defun make-cat (next size initial-element)
  (declare (type fixnum initial-element))
  (declare (type fixnum size))
  (let ((table (make-array size :initial-element 0)))
    (setf (aref table 0) initial-element)
    (%make-cat next table)))

(declaim (ftype (function (cat fixnum) fixnum) cat-sum))
;; L199 long sum(long)
(defun cat-sum (cat mask)
  (declare (type fixnum mask))
  (declare (optimize speed))
  (let ((sum (%cat-sum-cache cat)))
    (cond ((/= most-negative-fixnum sum)
           sum)
          (T
           (setf sum (if (null (%cat-next cat))
                         0
                         (cat-sum (%cat-next cat) mask)))
           (let ((%t (%cat-table cat)))
             (dotimes (i (length %t))
               (incf sum (logand (the fixnum (svref %t i)) (lognot mask))))
             (setf (%cat-sum-cache cat) sum)
             sum)))))

(declaim (ftype (function (cat fixnum) fixnum) cat-sum~))
;; L212 long estimate_sum(long)
(defun cat-sum~ (cat mask)
  (declare (type fixnum mask))
  (declare (optimize speed))
  (cond ((<= (length (%cat-table cat)) 64)
         (cat-sum cat mask))
        (T
         (let ((millis (get-internal-real-time)))
           (when (/= millis (%cat-fuzzy-time cat))
             (setf (%cat-fuzzy-sum-cache cat) (cat-sum cat mask))
             (setf (%cat-fuzzy-time cat) millis))
           (%cat-fuzzy-sum-cache cat)))))

;; The public interface.

(defstruct (counter (:constructor make-counter ())
                    (:conc-name %counter-))
  ;; Why is this slot at L97, after all the methods? I almost missed it, reading
  ;; the source code.
  (cat (make-cat NIL 4 0) :type cat))

(declaim (inline decf-counter))
;; L41 decrement(), but with a delta argument.
(defun decf-counter (counter &optional (delta 1))
  (declare (type fixnum delta))
  (counter-add-if-mask counter (- delta) 0))

(declaim (inline incf-counter))
;; L43 increment(), but with a delta increment.
(defun incf-counter (counter &optional (delta 1))
  (declare (type fixnum delta))
  (counter-add-if-mask counter delta 0))

;; L48 set(long)
(defun (setf counter-value) (x counter)
  (declare (optimize speed))
  (declare (type fixnum x))
  (loop with new = (make-cat NIL 4 x)
        until (cas (%counter-cat counter) (%counter-cat counter) new)))

(declaim (inline counter-value))
;; L59 get()
(defun counter-value (counter)
  (cat-sum (%counter-cat counter) 0))

(declaim (inline counter-value~))
;; L69 estimate_get()
(defun counter-value~ (counter)
  (cat-sum~ (%counter-cat counter) 0))

;; L150 add_if_mask(long, long, int, ConcurrentAutoTable)
;; This is a method in the CAT in the Java implementation, but here the
;; ConcurrentAutoTable (counter) is the object being acted upon.
(defun counter-add-if-mask (counter x mask)
  (declare (type fixnum x mask))
  (declare (optimize speed))
  (let* ((cat (%counter-cat counter))
         (%t (%cat-table cat))
         (idx (logand global-hash (1- (length %t))))
         (old (the fixnum (svref %t idx)))
         ;; Try once quickly
         (ok (cas (svref %t idx) (logand old (lognot mask)) (+ old x))))
    (flet ((fail () (return-from counter-add-if-mask old)))
      ;; Clear the cache
      (when (/= (%cat-sum-cache cat) most-negative-fixnum)
        (setf (%cat-sum-cache cat) most-negative-fixnum))
      (when ok (fail))
      (when (/= 0 (logand old mask)) (fail))
      ;; Try some more
      (let ((cnt 0))
        (declare (type fixnum cnt))
        (loop (setf old (the fixnum (svref %t idx)))
              (when (/= 0 (logand old mask)) (fail))
              (when (cas (svref %t idx) old (+ old x)) (return))
              (incf cnt))
        ;; Make sure we don't spin too long
        (when (< cnt MAX-SPIN) (fail))
        ;; Or grow too big
        (when (<= (* 1024 1024) (length %t)) (fail))
        ;; We are contending too much, increase the size in hopes it'll help
        (let ((r (%cat-resizers cat))
              (newbytes (ash (ash (length %t) 1) 4)))
          (declare (type fixnum r newbytes))
          (loop while (not (cas (%cat-resizers cat) r (+ r newbytes)))
                do (setf r (%cat-resizers cat)))
          (incf r newbytes)
          ;; Already doubled up, don't bother
          (unless (eql cat (%counter-cat counter))
            (fail))
          ;; Did we try to allocate too often already?
          (when (/= 0 (ash r -17))
            (sleep (/ (ash r -17) 1000))
            (unless (eql cat (%counter-cat counter))
              (fail)))
          ;; Try to extend the CAT once, if it fails another thread
          ;; already did it for us so we don't have to retry.
          (let ((new (make-cat cat (* (length %t) 2) 0)))
            (cas (%counter-cat counter) cat new)
            (fail)))))))
