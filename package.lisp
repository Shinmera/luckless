#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:luckless-tools
  (:nicknames #:org.shirakumo.luckless.tools)
  (:use #:cl)
  (:export
   #:cas
   #:atomic-incf))

;; FIXME: replace stars with shadowing

(defpackage #:luckless-list
  (:nicknames #:org.shirakumo.luckless.list)
  (:use #:cl #:org.shirakumo.luckless.tools)
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

(defpackage #:luckless-hashtable
  (:nicknames #:org.shirakumo.luckless.hashtable)
  (:use #:cl #:org.shirakumo.luckless.tools)
  (:export))
