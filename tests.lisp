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
  (equal list (luckless-list:to-list caslist)))

(define-test list-single-threaded
  :parent list
  (of-type luckless-list:caslist (luckless-list:caslist))
  (is eql NIL (luckless-list:first* (luckless-list:caslist)))
  (is = 1 (luckless-list:first* (luckless-list:caslist 1)))
  (is = 1 (luckless-list:nth* 0 (luckless-list:caslist 1)))
  (is = 3 (luckless-list:nth* 2 (luckless-list:caslist 1 2 3)))
  (is eql NIL (luckless-list:nth* 3 (luckless-list:caslist 1 2 3)))
  (is = 0 (luckless-list:length* (luckless-list:caslist)))
  (is = 3 (luckless-list:length* (luckless-list:caslist 1 2 3)))
  (is equal '() (luckless-list:to-list (luckless-list:caslist)))
  (is equal '(1 2 3) (luckless-list:to-list (luckless-list:caslist 1 2 3)))
  (is eql T (luckless-list:member* 1 (luckless-list:caslist 1 2 3)))
  (is eql T (luckless-list:member* 2 (luckless-list:caslist 1 2 3)))
  (is eql NIL (luckless-list:member* 5 (luckless-list:caslist 1 2 3)))
  (is list= '(0) (luckless-list:push* 0 (luckless-list:caslist)))
  (is list= '(0 1 2 3) (luckless-list:push* 0 (luckless-list:caslist 1 2 3)))
  (is list= '(0 1 2 3) (let ((l (luckless-list:caslist 1 2 3)))
                         (luckless-list:push* 0 l)
                         l))
  (is list= '(1 1 2 3) (luckless-list:push* 1 (luckless-list:caslist 1 2 3)))
  (is list= '(2 3) (luckless-list:delete* 1 (luckless-list:caslist 1 2 3)))
  (is list= '(1 3) (luckless-list:delete* 2 (luckless-list:caslist 1 2 3)))
  (is list= '(2 2) (luckless-list:delete* 2 (luckless-list:caslist 2 2 2)))
  (is list= '(1 2 3) (luckless-list:delete* 5 (luckless-list:caslist 1 2 3))))

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
           (let ((list (luckless-list:caslist)))
             (finish-threads
              (spawn-threads 2 (lambda ()
                                 (loop repeat 100000 do (luckless-list:push* 0 list)))))
             list))
         (make-delete-parallel ()
           (let ((list (luckless-list:caslist)))
             (finish-threads
              (spawn-threads 1 (lambda ()
                                  (loop repeat 100000 do (luckless-list:push* 0 list))))
              (spawn-threads 1 (lambda ()
                                  (loop repeat 100000 do (luckless-list:push* 1 list))))
              (spawn-threads 1 (lambda ()
                                  (loop repeat 100000 do (luckless-list:delete* 1 list)))))
             list)))
    (finish (make-list-parallel))
    (is = 200000 (luckless-list:length* (make-list-parallel)))
    (is = 0 (reduce #'+ (luckless-list:to-list (make-list-parallel))))
    (is = 0 (reduce #'+ (luckless-list:to-list (make-delete-parallel))))))
