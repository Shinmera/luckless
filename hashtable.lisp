#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>

 Based on:
   A Lock-Free Wait-Free Hash Table
   Cliff Click, Azul Systems, 2007
     https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/NonBlockingHashMap.java
|#

(in-package #:org.shirakumo.luckless.hashtable)

(defconstant max-spin 2)
(defconstant global-hash (sxhash (get-universal-time)))
(defconstant long-min (- (expt 2 63)))
(defconstant reprobe-limit 10)
(defconstant min-size-log 3)
(defconstant min-size (ash 1 min-size-log))
(defconstant no-match-old 'no-match-old)
(defconstant match-any 'match-any)
(defconstant tombstone 'tombstone)
(defconstant tombprime (if (boundp 'tombprime) tombprime (prime tombstone)))

(deftype long () '(signed-byte 64))
(deftype int () '(signed-byte 32))

(defun rehash (h)
  (incf h (logior (ash h 15) #xffffcd7d))
  (setf h (logior h (ash h -10)))
  (incf h (ash h 3))
  (setf h (logior h (ash h 6)))
  (incf h (+ (ash h 2) (ash h 14)))
  (logior h (ash h -16)))

(defstruct (cat
            (:constructor %make-cat (%next %t)))
  (%resizers 0 :type long)
  (%next NIL :type T)
  (%sum-cache LONG-MIN :type long)
  (%fuzzy-sum-cache 0 :type long)
  (%fuzzy-time 0 :type long)
  (%t NIL :type (simple-array long)))

(defun make-cat (next size initial-element)
  (declare (type long initial-element))
  (declare (type int size))
  (%make-cat next (make-array size :element-type 'long :initial-element initial-element)))

(defun cat-sum (cat mask)
  (declare (type long mask))
  (let ((sum (cat-%sum-cache cat)))
    (cond ((/= LONG-MIN sum) sum)
          (T
           (setf sum (if (cat-%next cat) (cat-sum (cat-%next cat) mask) 0))
           (let ((%t (cat-%t cat)))
             (dotimes (i (length %t))
               (incf sum (logand (svref %t i) (lognot mask))))
             (setf (cat-%sum-cache cat) sum)
             sum)))))

(defun cat-sum~ (cat mask)
  (cond ((<= (length (cat-%t cat)) 64)
         (cat-sum cat mask))
        (T
         (let ((millis (get-internal-real-time)))
           (when (/= millis (cat-%fuzzy-time cat))
             (setf (cat-%fuzzy-sum-cache cat) (cat-sum cat mask))
             (setf (cat-%fuzzy-time) millis))
           (cat-%fuzzy-sum-cache cat)))))

(defstruct (counter (:constructor make-counter ()))
  (%cat (make-cat NIL 4 0) :type cat))

(defun decf-counter (counter)
  (counter-add-if-mask counter -1 0))

(defun incf-counter (counter &optional (delta 1))
  (declare (type long delta))
  (counter-add-if-mask counter delta 0))

(defun (setf counter-value) (x counter)
  (declare (type long x))
  (loop with new = (make-cat NIL 4 x)
        until (cas (counter-%cat counter) (coutner-%cat counter) new)))

(defun counter-value (counter)
  (cat-sum (counter-%cat counter) 0))

(defun counter-value~ (counter)
  (cat-sum~ (counter-%cat counter) 0))

(defun counter-add-if-mask (counter x mask)
  (declare (type long x mask))
  (let* ((cat (counter-%cat counter))
         (%t (cat-%t cat))
         (idx (logand global-hash (1- (length %t))))
         (old (svref %t idx))
         (ok (cas (svref %t idx) (logand old (lognot mask)) (+ old x))))
    (flet ((done () (return-from counter-add-if-mask old)))
      (when (/= (cat-%sum-cache cat) LONG-MIN)
        (setf (cat-%sum-cache cat) LONG-MIN))
      (when ok (done))
      (when (/= 0 (logand old mask)) (done))
      (let ((cnt 0))
        (loop (setf old (svref %t idx))
              (when (/= 0 (logand old mask)) (done))
              (when (cas (svref %t idx) old (+ old x)) (return))
              (incf cnt))
        (when (< cnt MAX-SPIN) (done))
        (when (<= (* 1024 1024) (length %t)) (done))
        (let ((r (cat-%resizers cat))
              (newbytes (ash (ash (length %t) 1) 4)))
          (loop while (not (cas (cat-%resizers cat) r (+ r newbytes)))
                do (setf r (cat-%resizers cat)))
          (incf r newbytes)
          (unless (eql cat (counter-%cat counter))
            (done))
          (when (/= 0 (ash r -17))
            (sleep (ash r -17))
            (unless (eql cat (counter-%cat counter))
              (done)))
          (let ((new (make-cat cat (* (length %t) 2) 0)))
            (cat (counter-%cat counter) cat new)
            old))))))

(defstruct (prime
            (:constructor prime (value)))
  (value NIL :type T))

(defstruct (chm
            (:constructor make-chm (%size)))
  (%size NIL :type counter)
  (%slots (make-counter) :type counter)
  (%newkvs NIL :type (or null simple-vector))
  (%resizers 0 :type long))

(defun chm-size (chm)
  (counter-value (chm-%size chm)))

(defstruct (castable
            (:constructor %make-castable (%kvs %last-resize))
            (:conc-name NIL))
  (%kvs NIL :type simple-vector)
  (%last-resize NIL :type fixnum)
  (%reprobes (make-counter) :type counter))

(declaim (inline chm))
(declaim (ftype (function (simple-vector) chm) chm))
(defun chm (kvs)
  (svref kvs 0))

(declaim (inline hashes))
(declaim (ftype (function (simple-vector) (simple-array (unsigned-byte 32))) hashes))
(defun hashes (kvs)
  (svref kvs 1))

(declaim (inline len))
(declaim (ftype (function (simple-vector) (unsigned-byte 32)) len))
(defun len (kvs)
  (ash (- (length kvs) 2) -1))

(declaim (inline key))
(declaim (ftype (function (simple-vector (unsigned-byte 32)) T) key))
(defun key (kvs idx)
  (svref kvs (+ 2 (ash idx 1))))

(declaim (inline key))
(declaim (ftype (function (simple-vector (unsigned-byte 32)) T) val))
(defun val (kvs idx)
  (svref kvs (+ 3 (ash idx 1))))

(defun cas-key (kvs idx old key)
  (cas (svref kvs (+ 2 (ash idx 1))) old key))

(defun cas-val (kvs idx old val)
  (cas (svref kvs (+ 3 (ash idx 1))) old val))

(defun reprobes (table)
  (prog1 (counter-value (%reprobes table))
    (setf (%reprobes (make-counter)))))

(defun reprobe-limit (len)
  (+ REPROBE-LIMIT (ash len -2)))

(defun make-castable (&optional size)
  (let ((size (max MIN-SIZE (* 1024 1022) (or size 0))))
    (let ((i MIN-SIZE-LOG))
      (loop while (< (ash 1 i) (ash size 2))
            do (incf i))
      (let ((kvs (make-array (+ 2 (ash (ash 1 i) 1)))))
        (setf (svref kvs 0) (make-chm (make-counter)))
        (setf (svref kvs 1) (ash 1 i))
        (%make-castable kvs (get-internal-real-time))))))

(defun size (table)
  (chm-size (chm (%kvs table))))

(defun empty-p (table)
  (= 0 (size table)))

