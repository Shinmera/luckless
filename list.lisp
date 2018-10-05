#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:luckless-list
  (:nicknames #:org.shirakumo.luckless.list)
  (:use #:cl)
  (:export
   #:caslist
   #:to-list
   #:mapc*
   #:first*
   #:nth*
   #:length*
   #:push*
   #:delete*
   #:member*))
(in-package #:org.shirakumo.luckless.list)

(defstruct (cons*
            (:constructor cons* (car* cdr*))
            (:conc-name NIL))
  (car* NIL :type T)
  (cdr* NIL :type (or null cons*))
  (valid 1 :type (integer 0 1)))

(defmethod print-object ((cons cons*) stream)
  (print-unreadable-object (cons stream :type T :identity T)
    (format stream "~a ~[INVALID~;VALID~]" (car* cons) (valid cons))))

(defstruct (caslist
            (:constructor %make-caslist ())
            (:conc-name NIL))
  (head (cons* NIL NIL) :type cons*)
  (tail (cons* NIL NIL) :type cons*))

(defmethod print-object ((list caslist) stream)
  (print-unreadable-object (list stream :type T :identity T)
    (mapc* (lambda (value) (format stream "~a " value)) list)))

(defun caslist (&rest values)
  (let ((list (%make-caslist)))
    (setf (cdr* (head list)) (tail list))
    (loop for cons = (head list) then (cdr* cons)
          for value in values
          do (setf (cdr* cons) (cons* value (tail list))))
    list))

(defun first* (list)
  (mapc* (lambda (value) (return-from first* value))
         list)
  NIL)

(defun nth* (n list)
  (let ((i 0))
    (mapc* (lambda (value)
             (when (= i n)
               (return-from nth* value))
             (incf i))
           list)
    NIL))

(defun mapc* (function list)
  (declare (type caslist list))
  (loop for cons = (cdr* (head list)) then (cdr* cons)
        until (eq cons (tail list))
        do (when (= 1 (valid cons))
             (funcall function (car* cons))))
  list)

(defun length* (list)
  (let ((i 0))
    (mapc* (lambda (_)
             (declare (ignore _))
             (incf i))
           list)
    i))

(defun to-list (list)
  (declare (type caslist list))
  (let* ((sentinel (cons NIL NIL))
         (head sentinel))
    (mapc* (lambda (value)
             (setf sentinel
                   (setf (cdr sentinel) (list value))))
           list)
    (cdr head)))

;; FIXME: Lots of other ops to add: append, conc, mapcar, map-into, replace, etc.

(defun push* (value list)
  (declare (type caslist list))
  (let ((new (cons* value NIL))
        (left (head list)))
    (loop for right = (cdr* left)
          do (setf (cdr* new) right)
          until (eq (sb-ext:cas (cdr* left) right new)
                    right))
    list))

(defun delete* (value list)
  (declare (type caslist list))
  (let ((tail (tail list)))
    (loop (multiple-value-bind (right left) (search-cons value list)
            (when (or (eq right tail) (not (eql value (car* right))))
              (return list))
            (let ((next (cdr* right)))
              (when (and (= 1 (valid right))
                         (sb-ext:cas (valid right) 1 0))
                (unless (eq (sb-ext:cas (cdr* left) right next) right)
                  (search-cons (car* right) list))
                (return list)))))))

(defun member* (value list)
  (declare (type caslist list))
  (let ((right (search-cons value list)))
    (and (not (eq right (tail list)))
         (eql (car* right) value))))

(defun search-cons (value list)
  (declare (type caslist list))
  (let* ((tail (tail list))
         (right tail)
         (left tail)
         (left-next tail))
    (loop (let* ((cons (head list))
                 (next (cdr* cons)))
            (loop do (when (= 1 (valid cons))
                       (setf left cons)
                       (setf left-next next))
                     (setf cons next)
                     (when (eq cons tail)
                       (return))
                     (setf next (cdr* cons))
                  while (or (= 0 (valid cons))
                            (not (eql (car* cons) value))))
            (setf right cons)
            (cond ((eq left-next right)
                   (when (or (eq right tail)
                             (= 1 (valid right)))
                     (return (values right left))))
                  ((eq (sb-ext:cas (cdr* left) left-next right) left-next)
                   (when (or (eq right tail)
                             (= 1 (valid right)))
                     (return (values right left)))))))))
