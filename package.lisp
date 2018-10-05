#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:luckless
  (:nicknames #:org.shirakumo.luckless)
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
