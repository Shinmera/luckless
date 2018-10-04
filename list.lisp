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
   #:push*
   #:delete*
   #:member*))
(in-package #:luckless-list)

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
    (loop for cons = (cdr* (head list)) then (cdr* cons)
          until (eq cons (tail list))
          do (when (= 1 (valid cons))
               (format stream "~a " (car* cons))))))

(defun caslist (&rest values)
  (let ((list (%make-caslist)))
    (setf (cdr* (head list)) (tail list))
    (loop for cons = (head list) then (cdr* cons)
          for value in values
          do (setf (cdr* cons) (cons* value (tail list))))
    list))

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
              (return NIL))
            (let ((next (cdr* right)))
              (when (and (= 1 (valid right))
                         (sb-ext:cas (valid right) 1 0))
                (unless (sb-ext:cas (cdr* left) right next)
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
            (loop do (when (= 1 (valid next))
                       (setf left cons)
                       (setf left-next next))
                     (setf cons next)
                     (when (eq cons tail)
                       (return))
                     (setf next (cdr* cons))
                  while (or (= 0 (valid next))
                            (not (eql (car* cons) value))))
            (setf right cons)
            (cond ((eq left-next right)
                   (when (or (eq right tail)
                             (= 1 (valid (cdr* right))))
                     (return (values right left))))
                  ((eq (sb-ext:cas (cdr* left) left-next right) left-next)
                   (when (or (eq right tail)
                             (= 1 (valid (cdr* right))))
                     (return (values right left)))))))))
