#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:luckless-list
  (:nicknames #:org.shirakumo.luckless.list)
  (:use #:cl)
  (:import-from #:org.shirakumo.atomics #:cas)
  (:shadow #:mapc #:first #:nth #:length #:push #:delete #:member)
  (:export
   #:caslist
   #:caslist-p
   #:to-list
   #:mapc
   #:first
   #:nth
   #:length
   #:push
   #:delete
   #:member))

(defpackage #:luckless-hashtable
  (:nicknames #:org.shirakumo.luckless.hashtable)
  (:use #:cl)
  (:import-from #:org.shirakumo.atomics #:cas)
  (:shadow #:gethash #:remhash #:clrhash #:maphash #:count)
  (:export
   #:make-castable
   #:castable
   #:castable-p
   #:size
   #:count
   #:test
   #:hash-function
   #:gethash
   #:remhash
   #:clrhash
   #:maphash))
