#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>

 This implementation is based on
   A Pragmatic Implementation of Non-Blocking Linked-Lists
   Timothy L. Harris, Univeristy of Cambridge, 2001
     https://timharris.uk/papers/2001-disc.pdf
|#

(in-package #:org.shirakumo.luckless.list)

(defstruct (cons*
            (:constructor cons* (car* cdr*))
            (:conc-name NIL))
  (car* NIL :type T)
  (cdr* NIL :type (or null cons*))
  (valid 1 :type bit))

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
  (declare (optimize speed))
  (let ((list (%make-caslist)))
    (setf (cdr* (head list)) (tail list))
    (loop for cons = (head list) then (cdr* cons)
          for value in values
          do (setf (cdr* cons) (cons* value (tail list))))
    list))

(declaim (inline mapc*))
(defun mapc* (function list)
  (declare (type caslist list))
  (declare (optimize speed))
  (loop for cons = (cdr* (head list)) then (cdr* cons)
        until (eq cons (tail list))
        do (when (= 1 (valid cons))
             (funcall function (car* cons))))
  list)

(defun first* (list)
  (declare (optimize speed))
  (mapc* (lambda (value) (return-from first* value))
         list)
  NIL)

(defun nth* (n list)
  (declare (type unsigned-byte n))
  (declare (optimize speed))
  (let ((i 0))
    (mapc* (lambda (value)
             (when (= i n)
               (return-from nth* value))
             (incf i))
           list)
    NIL))

(defun length* (list)
  (declare (optimize speed))
  (let ((i 0))
    (mapc* (lambda (_)
             (declare (ignore _))
             (incf i))
           list)
    i))

(defun to-list (list)
  (declare (type caslist list))
  (declare (optimize speed))
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
  (declare (optimize speed))
  (let ((new (cons* value NIL))
        (left (head list)))
    ;; FIXME: maybe search-cons for first would be better so that push can help
    ;;        clear up invalid conses.
    (loop for right = (cdr* left)
          do (setf (cdr* new) right)
          until (cas (cdr* left) right new))
    list))

(defun delete* (value list)
  (declare (type caslist list))
  (declare (optimize speed))
  (let ((tail (tail list)))
    (loop (multiple-value-bind (right left) (search-cons value list)
            (when (or (eq right tail) (not (eql value (car* right))))
              (return list))
            (let ((next (cdr* right)))
              (when (and (= 1 (valid right))
                         (cas (valid right) 1 0))
                (unless (cas (cdr* left) right next)
                  (search-cons (car* right) list))
                (return list)))))))

(defun member* (value list)
  (declare (type caslist list))
  (declare (optimize speed))
  (let ((right (search-cons value list)))
    (and (not (eq right (tail list)))
         (eql (car* right) value))))

(defun search-cons (value list)
  (declare (type caslist list))
  (declare (optimize speed (safety 0)))
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
            (when (or (eq left-next right)
                      (cas (cdr* left) left-next right))
              (when (or (eq right tail)
                        (= 1 (valid right)))
                (return (values right left))))))))
