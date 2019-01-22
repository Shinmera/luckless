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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (prime
              (:constructor prime (value)))
    (value NIL :type T)))

(defconstant max-spin 2)
(defconstant global-hash (if (boundp 'global-hash)
                             global-hash
                             #.(sxhash (get-universal-time))))
(defconstant reprobe-limit 10)
(defconstant min-size-log 3)
(defconstant min-size (ash 1 min-size-log))
(defconstant no-match-old 'no-match-old)
(defconstant match-any 'match-any)
(defconstant tombstone 'tombstone)
(defconstant tombprime (if (boundp 'tombprime) tombprime (prime tombstone)))
(defconstant no-value 'no-value)

(defun rehash (h)
  (incf h (logior (ash h 15) #xffffcd7d))
  (setf h (logior h (ash h -10)))
  (incf h (ash h 3))
  (setf h (logior h (ash h -6)))
  (incf h (+ (ash h 2) (ash h 14)))
  (logior h (ash h -16)))

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
  (%make-cat next (make-array size :initial-element initial-element)))

(defun cat-sum (cat mask)
  (declare (type fixnum mask))
  (let ((sum (%cat-sum-cache cat)))
    (cond ((/= most-negative-fixnum sum) sum)
          (T
           (setf sum (if (%cat-next cat) (cat-sum (%cat-next cat) mask) 0))
           (let ((%t (%cat-table cat)))
             (dotimes (i (length %t))
               (incf sum (logand (the fixnum (svref %t i)) (lognot mask))))
             (setf (%cat-sum-cache cat) sum)
             sum)))))

(defun cat-sum~ (cat mask)
  (cond ((<= (length (%cat-table cat)) 64)
         (cat-sum cat mask))
        (T
         (let ((millis (get-internal-real-time)))
           (when (/= millis (%cat-fuzzy-time cat))
             (setf (%cat-fuzzy-sum-cache cat) (cat-sum cat mask))
             (setf (%cat-fuzzy-time cat) millis))
           (%cat-fuzzy-sum-cache cat)))))

(defstruct (counter (:constructor make-counter ())
                    (:conc-name %counter-))
  (cat (make-cat NIL 4 0) :type cat))

(defun decf-counter (counter)
  (counter-add-if-mask counter -1 0))

(defun incf-counter (counter &optional (delta 1))
  (declare (type fixnum delta))
  (counter-add-if-mask counter delta 0))

(defun (setf counter-value) (x counter)
  (declare (type fixnum x))
  (loop with new = (make-cat NIL 4 x)
        until (cas (%counter-cat counter) (%counter-cat counter) new)))

(defun counter-value (counter)
  (cat-sum (%counter-cat counter) 0))

(defun counter-value~ (counter)
  (cat-sum~ (%counter-cat counter) 0))

(defun counter-add-if-mask (counter x mask)
  (declare (type fixnum x mask))
  (let* ((cat (%counter-cat counter))
         (%t (%cat-table cat))
         (idx (logand global-hash (1- (length %t))))
         (old (svref %t idx))
         (ok (cas (svref %t idx) (logand old (lognot mask)) (+ old x))))
    (flet ((done () (return-from counter-add-if-mask old)))
      (when (/= (%cat-sum-cache cat) most-negative-fixnum)
        (setf (%cat-sum-cache cat) most-negative-fixnum))
      (when ok (done))
      (when (/= 0 (logand old mask)) (done))
      (let ((cnt 0))
        (loop (setf old (svref %t idx))
              (when (/= 0 (logand old mask)) (done))
              (when (cas (svref %t idx) old (+ old x)) (return))
              (incf cnt))
        (when (< cnt MAX-SPIN) (done))
        (when (<= (* 1024 1024) (length %t)) (done))
        (let ((r (%cat-resizers cat))
              (newbytes (ash (ash (length %t) 1) 4)))
          (loop while (not (cas (%cat-resizers cat) r (+ r newbytes)))
                do (setf r (%cat-resizers cat)))
          (incf r newbytes)
          (unless (eql cat (%counter-cat counter))
            (done))
          (when (/= 0 (ash r -17))
            (sleep (ash r -17))
            (unless (eql cat (%counter-cat counter))
              (done)))
          (let ((new (make-cat cat (* (length %t) 2) 0)))
            (cas (%counter-cat counter) cat new)
            old))))))

(defstruct (chm
            (:constructor make-chm (size))
            (:conc-name %chm-))
  (size NIL :type counter)
  (slots (make-counter) :type counter)
  (newkvs NIL :type (or null simple-vector))
  (resizers 0 :type fixnum)
  (copy-idx 0 :type fixnum)
  (copy-done 0 :type fixnum))

(defun chm-size (chm)
  (counter-value (%chm-size chm)))

(defun chm-slots (chm)
  (counter-value (%chm-slots chm)))

(defun cas-newkvs (chm newkvs)
  (loop until (%chm-newkvs chm)
        do (when (cas (%chm-newkvs chm) NIL newkvs)
             (return T))
        finally (return NIL)))

;; Heuristic to test if the table is too full and we should make a new one.
(defun table-full-p (chm reprobe-cnt len)
  (and (<= REPROBE-LIMIT reprobe-cnt)
       (<= (reprobe-limit len) (counter-value~ (%chm-slots chm)))))

(defstruct (castable
            (:constructor %make-castable (kvs last-resize test hasher))
            (:conc-name %castable-))
  (kvs NIL :type simple-vector)
  (last-resize NIL :type fixnum)
  (reprobes (make-counter) :type counter)
  (test NIL :type (function (T T) boolean))
  (hasher NIL :type (function (T) fixnum)))

(declaim (inline chm))
(declaim (ftype (function (simple-vector) chm) chm))
(defun chm (kvs)
  (svref kvs 0))

(declaim (inline hashes))
(declaim (ftype (function (simple-vector) (simple-array fixnum)) hashes))
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
  (prog1 (counter-value (%castable-reprobes table))
    (setf (%castable-reprobes table) (make-counter))))

(defun reprobe-limit (len)
  (+ REPROBE-LIMIT (ash len -2)))

(defun determine-hasher (test)
  (or (cond ((eq test #'eq)
             #+sbcl #'sb-impl::eq-hash
             #-sbcl #'sxhash)
            ((eq test #'eql)
             #+sbcl #'sb-impl::eql-hash
             #-sbcl #'sxhash)
            ((eq test #'equal)
             #+sbcl #'sb-impl::equal-hash
             #-sbcl #'sxhash)
            ((eq test #'equalp)
             #+sbcl #'sb-impl::equalp-hash
             ;; FIXME: implement own equalp hash
             )
            (T
             #+sbcl (third (find test sb-impl::*user-hash-table-tests* :key #'second))))
      (error "Don't know a hasher for ~a." test)))

(defun make-castable (&key test size hasher)
  (let* ((size (max MIN-SIZE (* 1024 1022) (or size 0)))
         (test (etypecase test
                 (null #'eql)
                 (function test)
                 (symbol (fdefinition test))))
         (hasher (etypecase hasher
                   (null (determine-hasher test))
                   (function hasher)
                   (symbol (fdefinition hasher)))))
    (let ((i MIN-SIZE-LOG))
      (loop while (< (ash 1 i) (ash size 2))
            do (incf i))
      (let ((kvs (make-array (+ 2 (ash (ash 1 i) 1)) :initial-element NO-VALUE)))
        (setf (svref kvs 0) (make-chm (make-counter)))
        (setf (svref kvs 1) (make-array (ash 1 i) :element-type 'fixnum :initial-element 0))
        (%make-castable kvs (get-internal-real-time) test hasher)))))

(defun hash (table thing)
  (rehash (funcall (%castable-hasher table) thing)))

(defun size (table)
  (chm-size (chm (%castable-kvs table))))

(defun empty-p (table)
  (= 0 (size table)))

;; Missing: putIfAbsent
;; Missing: containsValue
;; Missing: replace
;; Missing: clone
;; Missing: iteration

(defun (setf gethash*) (value key table)
  (put-if-match table key value NO-MATCH-OLD))

(defun remhash* (table key)
  (put-if-match table key TOMBSTONE NO-MATCH-OLD))

(defun try-remhash (table key val &optional (test #'eql))
  (let ((out (put-if-match table key TOMBSTONE val)))
    (funcall test out val)))

(defun put-if-match (table key new old)
  (let ((res (%put-if-match table (%castable-kvs table) key new old)))
    (check-type res (not prime))
    (assert (not (eq res NO-VALUE)))
    (unless (eq res TOMBSTONE)
      res)))

(defun clrhash* (table)
  (let ((new (%castable-kvs (make-castable))))
    (loop until (cas (%castable-kvs table) (%castable-kvs table) new))))

(defun keyeq (k key hashes hash fullhash test)
  (or (eq k key)
      ;; Key does not match exactly, so try more expensive comparison.
      (and ;; If the hash exists, does it match?
           (or (= (svref hashes hash) 0)
               (= (svref hashes hash) fullhash))
           ;; Avoid testing tombstones
           (not (eq k TOMBSTONE))
           ;; Call test function for real comparison
           (funcall test key k))))

(defun gethash* (key table &optional default)
  (let* ((fullhash (hash table key))
         (value (%gethash table (%castable-kvs table) key fullhash)))
    ;; Make sure we never return primes
    (check-type value (not prime))
    (if (eql value NO-VALUE)
        default
        value)))

(defun %gethash (table kvs key fullhash)
  (let* ((len (len kvs))
         (chm (chm kvs))
         (hashes (hashes kvs))
         (idx (logand fullhash (1- len)))
         (test (%castable-test table))
         (reprobe-cnt 0))
    ;; Spin for a hit
    (loop (let ((k (key kvs idx))
                (v (val kvs idx)))
            ;; Early table miss
            (when (eq k NO-VALUE) (return NO-VALUE))
            (let ((newkvs (%chm-newkvs chm)))
              ;; Compare the keys
              (when (keyeq k key hashes idx fullhash test)
                ;; If we are not copying at the moment, we're done.
                (unless (typep v 'prime)
                  (return (if (eq v TOMBSTONE)
                              NO-VALUE
                              v)))
                ;; Copy in progress, help with copying and retry.
                (return (%gethash table
                                   (copy-slot-and-check chm table kvs idx key)
                                   key
                                   fullhash)))
              ;; If we exceed reprobes, help resizing.
              (when (or (<= (reprobe-limit len) (incf reprobe-cnt))
                        (eq key TOMBSTONE))
                (return (when newkvs
                          ;; Retry in a new table copy
                          (%gethash table (help-copy table newkvs) key fullhash))))
              ;; Reprobe.
              (setf idx (logand (1+ idx) (1- len))))))))

(defun %put-if-match (table kvs key put exp)
  (let* ((fullhash (hash table key))
         (len (len kvs))
         (chm (chm kvs))
         (hashes (hashes kvs))
         (test (%castable-test table))
         (idx (logand fullhash (1- len)))
         (reprobe-cnt 0)
         (k NO-VALUE) (v NO-VALUE)
         (newkvs NIL))
    ;; Spin for a hit
    (loop (setf v (val kvs idx))
          (setf k (key kvs idx))
          ;; Is the slot free?
          (when (eq k NO-VALUE)
            ;; No need to put a tombstone in an empty field
            (when (eq put TOMBSTONE)
              (return-from %put-if-match put))
            ;; Claim the spot
            (when (cas (svref kvs idx) NO-VALUE key)
              (incf-counter (chm-slots chm))
              (setf (aref hashes idx) fullhash)
              (return))
            ;; We failed, update the key
            (setf k (key kvs idx))
            (assert (not (eq k NO-VALUE))))
          ;; Okey, we have a key there
          (setf newkvs (%chm-newkvs chm))
          ;; Test if this is our key
          (when (keyeq k key hashes idx fullhash test)
            (return))
          ;; If we exceed reprobes, start resizing
          (when (or (<= (reprobe-limit len) (incf reprobe-cnt))
                    (eq key TOMBSTONE))
            (setf newkvs (resize chm table kvs))
            (unless (eq exp NO-VALUE) (help-copy table newkvs))
            (return-from %put-if-match
              (%put-if-match table newkvs key put exp)))
          ;; Reprobe.
          (setf idx (logand (1+ idx) (1- len))))
    ;; We found a key slot, time to update it
    ;; Fast-path
    (when (eq put v) (return-from %put-if-match v))
    ;; Check if we want to move to a new table
    (when (and ;; Do we have a new table already?
               (null newkvs)
               ;; Check the value
               (or (and (eq v NO-VALUE) (table-full-p chm reprobe-cnt len))
                   (typep v 'prime)))
      (setf newkvs (resize chm table kvs)))
    ;; Check if we are indeed moving and retry
    (when newkvs
      (return-from %put-if-match
        (%put-if-match table (copy-slot-and-check chm table kvs idx exp) key put exp)))
    ;; Finally we can do the update
    (loop (check-type v (not prime))
          ;; If we don't match the old, bail out
          (when (and (not (eq exp NO-MATCH-OLD))
                     (not (eq v exp))
                     (or (not (eq exp MATCH-ANY))
                         (eq v TOMBSTONE)
                         (eq v NO-VALUE))
                     (not (and (eq v NO-VALUE) (eq exp TOMBSTONE)))
                     (or (eq exp NO-VALUE) (not (funcall test exp v))))
            (return v))
          ;; Perform the change
          (when (cas (svref kvs idx) v put)
            ;; Okey, we got it, update the size
            (when (and (or (eq v NO-VALUE) (eq v TOMBSTONE))
                       (not (eq put TOMBSTONE)))
              (incf-counter (chm-size chm)))
            (when (and (not (or (eq v NO-VALUE) (eq v TOMBSTONE)))
                       (eq put TOMBSTONE))
              (decf-counter (chm-size chm)))
            (return (if (and (eq v NO-VALUE) (not (eq exp NO-VALUE)))
                        TOMBSTONE
                        v)))
          ;; CAS failed, retry
          (setf v (val kvs idx))
          ;; If we got a prime we need to restart from the beginning
          (when (typep v 'prime)
            (return (%put-if-match table (copy-slot-and-check chm table kvs idx exp) key put exp))))))

(defun help-copy (table helper)
  (let* ((topkvs (%castable-kvs table))
         (topchm (chm topkvs)))
    (when (%chm-newkvs topchm)
      (%help-copy topchm table topkvs NIL))
    helper))

(defun resize (chm table kvs)
  (assert (eq chm (chm kvs)))
  ;; Check for resize in progress
  (let ((newkvs (%chm-newkvs chm)))
    (when newkvs (return-from resize newkvs))
    (let* ((oldlen (len kvs))
           (sz (chm-size chm))
           (newsz sz))
      ;; Heuristic for new size
      (when (<= (ash oldlen -2) sz)
        (setf newsz (ash oldlen 1))
        (when (<= sz (ash oldlen -1))
          (setf newsz (ash oldlen 2))))
      ;; Much denser table with more reprobes
      #+(or)
      (when (<= (ash oldlen -1) sz)
        (setf newsz (ash oldlen 1)))
      ;; Was the last resize recent? If so, double again
      ;; to accommodate tables with lots of inserts at the moment.
      (let ((tm (get-internal-real-time)))
        (when (and (<= newsz oldlen)
                   ;; If we resized less than a second ago
                   (<= tm (+ (%castable-last-resize table)
                             INTERNAL-TIME-UNITS-PER-SECOND))
                   ;; And we have plenty of dead keys
                   (<= (ash sz 1) (counter-value~ (%chm-slots chm))))
          (setf newsz (ash oldlen 1))))
      ;; Don't shrink
      (when (< newsz oldlen) (setf newsz oldlen))
      (let ((log2 MIN-SIZE-LOG)
            (r (%chm-resizers chm)))
        ;; Convert to power of two
        (loop while (< (ash 1 log2) newsz)
              do (incf log2))
        ;; Limit the number of threads resizing things
        (loop until (cas (%chm-resizers chm) r (1+ r))
              do (setf r (%chm-resizers chm)))
        ;; Size calculation: 2 words per table + extra
        ;; NOTE: The original assumes 32 bit pointers, we conditionalise
        (let ((megs (ash (ash (+ (* (ash 1 log2) 2) 4)
                              #+64-BIT 4 #-64-BIT 3)
                         -20)))
          (when (and (<= 2 r) (< 0 megs))
            (setf newkvs (%chm-newkvs chm))
            (when newkvs
              (return-from resize newkvs))
            ;; We already have two threads trying a resize, wait
            (sleep (/ (* 8 megs) 1000))))
        ;; Last check
        (setf newkvs (%chm-newkvs chm))
        (when newkvs
          (return-from resize newkvs))
        ;; Allocate the array
        (setf newkvs (make-array (+ 2 (* 2 (ash 1 log2))) :initial-element NO-VALUE))
        (setf (svref newkvs 0) (make-chm (%chm-size chm)))
        (setf (svref newkvs 1) (make-array (ash 1 log2) :element-type 'fixnum :initial-element 0))
        ;; Check again after the allocation
        (when (%chm-newkvs chm)
          (return-from resize (%chm-newkvs chm)))
        ;; CAS the table in. We can let the GC handle deallocation. Thanks, GC!
        (if (cas-newkvs chm newkvs)
            newkvs
            (%chm-newkvs chm))))))

(defun %help-copy (chm table oldkvs copy-all)
  (assert (eq chm (chm oldkvs)))
  (let* ((newkvs (%chm-newkvs chm))
         (oldlen (len oldkvs))
         (min-copy-work (min oldlen 1024))
         (panic-start -1)
         (copy-idx -9999))
    (assert (not (null newkvs)))
    ;; Loop while there's work to be done
    (loop while (< (%chm-copy-done chm) oldlen)
          do ;; We panic if we tried to copy twice and it failed.
          (when (= -1 panic-start)
            (setf copy-idx (%chm-copy-idx chm))
            (loop while (and (< copy-idx (ash oldlen 1))
                             (not (cas (%chm-copy-idx chm) copy-idx (+ copy-idx min-copy-work))))
                  do (setf copy-idx (%chm-copy-idx chm)))
            (unless (< copy-idx (ash oldlen 1))
              (setf panic-start copy-idx)))
          ;; Okey, now perform the copy.
          (let ((workdone 0))
            (dotimes (i min-copy-work)
              (when (copy-slot table (logand (+ copy-idx i) (1- oldlen)) oldkvs newkvs)
                (incf workdone)))
            ;; Promote our work
            (when (< 0 workdone)
              (copy-check-and-promote chm table oldkvs workdone))
            (incf copy-idx min-copy-work)
            ;; End early if we shouldn't copy everything.
            (when (and (not copy-all) (= -1 panic-start))
              (return-from %help-copy))))
    ;; Promote again in case we race on end of copy
    (copy-check-and-promote chm table oldkvs 0)))

;; Copy the slot and check that we have done so successfully.
(defun copy-slot-and-check (chm table oldkvs idx should-help)
  (assert (eq chm (chm oldkvs)))
  (let ((newkvs (%chm-newkvs chm)))
    (assert (not (null newkvs)))
    (when (copy-slot table idx oldkvs (%chm-newkvs chm))
      (copy-check-and-promote chm table oldkvs 1))
    (if should-help
        (help-copy table newkvs)
        newkvs)))

(defun copy-check-and-promote (chm table oldkvs work-done)
  (assert (eq chm (chm oldkvs)))
  (let ((oldlen (len oldkvs))
        (copy-done (%chm-copy-done chm)))
    (assert (<= (+ copy-done work-done) oldlen))
    (when (< 0 work-done)
      (loop until (cas (%chm-copy-done chm) copy-done (+ copy-done work-done))
            do (setf copy-done (%chm-copy-done chm))
               (assert (<= (+ copy-done work-done) oldlen))))
    ;; Check for copy being completely done and promote
    (when (and (= (+ copy-done work-done) oldlen)
               (eq (%castable-kvs table) oldkvs)
               (cas (%castable-kvs table) oldkvs (%chm-newkvs chm)))
      (setf (%castable-last-resize table) (get-internal-real-time)))))

;; Copy one slot into the new table,
;; returns true if we are sure that the new table has a value.
(defun copy-slot (table idx oldkvs newkvs)
  ;; First tombstone the key blindly.
  (let (key)
    (loop while (eq (setf key (key oldkvs idx)) NO-VALUE)
          do (cas-key oldkvs idx NO-VALUE TOMBSTONE))
    ;; Prevent new values from showing up in the old table
    (let ((oldval (val oldkvs idx)))
      (loop until (typep oldval 'prime)
            for box = (if (or (eq oldval NO-VALUE)
                              (eq oldval TOMBSTONE))
                          TOMBPRIME
                          (prime oldval))
            do (when (cas-val oldkvs idx oldval box)
                 ;; We made sure to prime the value to prevent updates.
                 (when (eq box TOMBPRIME)
                   (return-from copy-slot T))
                 (setf oldval box)
                 (return))
               ;; Retry on CAS failure
               (setf oldval (val oldkvs idx)))
      (when (eq oldval TOMBPRIME)
        ;; We already completed the copy
        (return-from copy-slot NIL))
      ;; Finally do the actual copy, but only if we would write into
      ;; a null. Otherwise, someone else already copied.
      (let ((old-unboxed (prime-value oldval)))
        (assert (not (eq old-unboxed TOMBSTONE)))
        (prog1 (eq NO-VALUE (%put-if-match table newkvs key old-unboxed NO-VALUE))
          ;; Now that the copy is done, we can stub out the old key completely.
          (loop until (cas-val oldkvs idx oldval TOMBPRIME)
                do (setf oldval (val oldkvs idx))))))))
