#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:luckless-test
  (:nicknames #:org.shirakumo.luckless.test)
  (:local-nicknames
   (#:caslist #:org.shirakumo.luckless.list)
   (#:castable #:org.shirakumo.luckless.hashtable))
  (:use #:cl #:parachute)
  (:export
   #:luckless
   #:caslist
   #:caslist-single-threaded
   #:caslist-multi-threaded
   #:castable
   #:castable-single-threaded
   #:castable-multi-threaded))
(in-package #:org.shirakumo.luckless.test)

(defun spawn-threads (n function)
  (loop for i from 0 below n
        collect (bt:make-thread function :name (format NIL "~dth test thread" i))))

(defun finish-threads (&rest threads)
  (loop for thread in threads
        do (if (listp thread)
               (mapc #'bt:join-thread thread)
               (bt:join-thread thread))))

(define-test luckless)

(define-test caslist
  :parent luckless)

(defun list= (caslist list)
  (equal list (caslist:to-list caslist)))

(define-test caslist-single-threaded
  :parent caslist
  (of-type caslist:caslist (caslist:caslist))
  (is eql NIL (caslist:first* (caslist:caslist)))
  (is = 1 (caslist:first* (caslist:caslist 1)))
  (is = 1 (caslist:nth* 0 (caslist:caslist 1)))
  (is = 3 (caslist:nth* 2 (caslist:caslist 1 2 3)))
  (is eql NIL (caslist:nth* 3 (caslist:caslist 1 2 3)))
  (is = 0 (caslist:length* (caslist:caslist)))
  (is = 3 (caslist:length* (caslist:caslist 1 2 3)))
  (is equal '() (caslist:to-list (caslist:caslist)))
  (is equal '(1 2 3) (caslist:to-list (caslist:caslist 1 2 3)))
  (is eql T (caslist:member* 1 (caslist:caslist 1 2 3)))
  (is eql T (caslist:member* 2 (caslist:caslist 1 2 3)))
  (is eql NIL (caslist:member* 5 (caslist:caslist 1 2 3)))
  (is list= '(0) (caslist:push* 0 (caslist:caslist)))
  (is list= '(0 1 2 3) (caslist:push* 0 (caslist:caslist 1 2 3)))
  (is list= '(0 1 2 3) (let ((l (caslist:caslist 1 2 3)))
                         (caslist:push* 0 l)
                         l))
  (is list= '(1 1 2 3) (caslist:push* 1 (caslist:caslist 1 2 3)))
  (is list= '(2 3) (caslist:delete* 1 (caslist:caslist 1 2 3)))
  (is list= '(1 3) (caslist:delete* 2 (caslist:caslist 1 2 3)))
  (is list= '(2 2) (caslist:delete* 2 (caslist:caslist 2 2 2)))
  (is list= '(1 2 3) (caslist:delete* 5 (caslist:caslist 1 2 3))))

(define-test caslist-multi-threaded
  :parent caslist
  :depends-on (caslist-single-threaded)
  (flet ((make-list-parallel ()
           (let ((list (caslist:caslist)))
             (finish-threads
              (spawn-threads 2 (lambda () (loop repeat 100000 do (caslist:push* 0 list)))))
             list))
         (make-delete-parallel ()
           (let ((list (caslist:caslist)))
             (finish-threads
              (spawn-threads 1 (lambda () (loop repeat 100000 do (caslist:push* 0 list))))
              (spawn-threads 1 (lambda () (loop repeat 100000 do (caslist:push* 1 list))))
              (spawn-threads 1 (lambda () (loop repeat 100000 do (caslist:delete* 1 list)))))
             list)))
    (finish (make-list-parallel))
    (is = 200000 (caslist:length* (make-list-parallel)))
    (is = 0 (reduce #'+ (caslist:to-list (make-list-parallel))))
    (is = 0 (reduce #'+ (caslist:to-list (make-delete-parallel))))))

(define-test castable
  :parent luckless)

(define-test castable-single-threaded
  :parent castable
  (of-type castable:castable (castable:make-castable))
  (finish (castable:clrhash* (castable:make-castable)))
  (is eql NIL (castable:gethash* NIL (castable:make-castable)))
  (is eql NIL (castable:gethash* T (castable:make-castable)))
  (is eql T (castable:gethash* NIL (castable:make-castable) T))
  (is eql T (setf (castable:gethash* T (castable:make-castable)) T))
  (is eql NIL (castable:remhash* T (castable:make-castable)))
  (let ((table (castable:make-castable)))
    (is eql T (setf (castable:gethash* T table) T))
    (is eql T (castable:gethash* T table))
    (is eql T (castable:remhash* T Table))
    (is eql NIL (castable:gethash* T table))
    (is eql NIL (castable:remhash* T Table)))
  (let ((table (castable:make-castable)))
    (finish (dotimes (i 100) (setf (castable:gethash* i table) i)))
    (is eql T (loop for i from 0 below 100 always (= i (castable:gethash* i table))))
    (is = 100 (castable:size table))))

(define-test castable-multi-threaded
  :parent castable
  :depends-on (castable-single-threaded)
  )
