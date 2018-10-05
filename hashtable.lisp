#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>

 Based on:
   A Lock-Free Wait-Free Hash Table
   Cliff Click, Azul Systems, 2007
     https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/NonBlockingHashMap.java
|#

(in-package #:org.shirakumo.luckless)

(defun rehash (h)
  (incf h (logior (ash h 15) #xffffcd7d))
  (setf h (logior h (ash h -10)))
  (incf h (ash h 3))
  (setf h (logior h (ash h 6)))
  (incf h (+ (ash h 2) (ash h 14)))
  (logior h (ash h -16)))

(defstruct (counter))

(defstruct (prime
            (:constructor prime (value)))
  (value NIL :type T))

(defconstant min-size-log 3)
(defconstant min-size (ash 1 min-size-log))
(defconstant no-match-old 'no-match-old)
(defconstant match-any 'match-any)
(defconstant tombstone 'tombstone)
(defconstant tombprime (if (boundp 'tombprime) tombprime (prime tombstone)))

(defstruct (chm
            (:constructor make-chm (%size)))
  (%size NIL :type counter)
  (%slots (make-counter) :type counter)
  (%newkvs NIL :type (or null simple-vector))
  (%resizers 0 :type (unsigned-byte 32)))

(defstruct (castable
            (:constructor %make-castable (%kvs %last-resize))
            (:conc-name NIL))
  (%kvs NIL :type simple-vector)
  (%last-resize NIL :type fixnum))

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

