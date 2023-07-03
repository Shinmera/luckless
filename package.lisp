(defpackage #:org.shirakumo.luckless.list
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

(defpackage #:org.shirakumo.luckless.hashtable
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
   #:try-remhash
   #:put-if-absent
   #:put-if-equal
   #:put-if-present
   #:clrhash
   #:maphash))

(defpackage #:org.shirakumo.luckless.queue
  (:use #:cl)
  (:import-from #:org.shirakumo.atomics #:cas)
  (:shadow #:push #:mapc #:length)
  (:export
   #:queue
   #:make-queue
   #:queue-p
   #:push
   #:discard
   #:mapc
   #:length))
