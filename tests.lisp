#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:luckless-test
  (:nicknames #:org.shirakumo.luckless.test)
  (:use #:cl #:parachute))
(in-package #:org.shirakumo.luckless.test)

(define-test luckless)

(define-test list
  :parent luckless)

(defun list= (caslist list)
  (equal list (luckless:to-list caslist)))

(define-test list-single-threaded
  :parent list
  (of-type luckless:caslist (luckless:caslist))
  (is eql NIL (luckless:first* (luckless:caslist)))
  (is = 1 (luckless:first* (luckless:caslist 1)))
  (is = 1 (luckless:nth* 0 (luckless:caslist 1)))
  (is = 3 (luckless:nth* 2 (luckless:caslist 1 2 3)))
  (is eql NIL (luckless:nth* 3 (luckless:caslist 1 2 3)))
  (is = 0 (luckless:length* (luckless:caslist)))
  (is = 3 (luckless:length* (luckless:caslist 1 2 3)))
  (is equal '() (luckless:to-list (luckless:caslist)))
  (is equal '(1 2 3) (luckless:to-list (luckless:caslist 1 2 3)))
  (is eql T (luckless:member* 1 (luckless:caslist 1 2 3)))
  (is eql T (luckless:member* 2 (luckless:caslist 1 2 3)))
  (is eql NIL (luckless:member* 5 (luckless:caslist 1 2 3)))
  (is list= '(0) (luckless:push* 0 (luckless:caslist)))
  (is list= '(0 1 2 3) (luckless:push* 0 (luckless:caslist 1 2 3)))
  (is list= '(0 1 2 3) (let ((l (luckless:caslist 1 2 3)))
                         (luckless:push* 0 l)
                         l))
  (is list= '(1 1 2 3) (luckless:push* 1 (luckless:caslist 1 2 3)))
  (is list= '(2 3) (luckless:delete* 1 (luckless:caslist 1 2 3)))
  (is list= '(1 3) (luckless:delete* 2 (luckless:caslist 1 2 3)))
  (is list= '(2 2) (luckless:delete* 2 (luckless:caslist 2 2 2)))
  (is list= '(1 2 3) (luckless:delete* 5 (luckless:caslist 1 2 3))))

(defun spawn-threads (n function)
  (loop for i from 0 below n
        collect (bt:make-thread function :name (format NIL "~dth test thread" i))))

(defun finish-threads (&rest threads)
  (loop for thread in threads
        do (if (listp thread)
               (mapc #'bt:join-thread thread)
               (bt:join-thread thread))))

(define-test list-multi-threaded
  :parent list
  :depends-on (list-single-threaded)
  (flet ((make-list-parallel ()
           (let ((list (luckless:caslist)))
             (finish-threads
              (spawn-threads 2 (lambda () (loop repeat 100000 do (luckless:push* 0 list)))))
             list))
         (make-delete-parallel ()
           (let ((list (luckless:caslist)))
             (finish-threads
              (spawn-threads 1 (lambda () (loop repeat 100000 do (luckless:push* 0 list))))
              (spawn-threads 1 (lambda () (loop repeat 100000 do (luckless:push* 1 list))))
              (spawn-threads 1 (lambda () (loop repeat 100000 do (luckless:delete* 1 list)))))
             list)))
    (finish (make-list-parallel))
    (is = 200000 (luckless:length* (make-list-parallel)))
    (is = 0 (reduce #'+ (luckless:to-list (make-list-parallel))))
    (is = 0 (reduce #'+ (luckless:to-list (make-delete-parallel))))))
