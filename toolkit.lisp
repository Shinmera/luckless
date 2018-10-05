#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.luckless)

(defmacro cas (place old new)
  ;; WARNING: This is for internal use ONLY.
  ;;          It makes some assumptions about the arguments for speed reasons.
  ;;          Most importantly, PLACE must be a struct slot accessor function.
  #+allegro
  `(excl:atomic-conditional-setf ,place ,new ,old)
  #+ccl
  `(ccl::conditional-store ,place ,old ,new)
  #+ecl
  (let ((tmp (gensym "OLD")))
    `(let ((,tmp ,old)) (eq ,tmp (mp:compare-and-swap ,place ,tmp ,new))))
  #+lispworks
  `(system:compare-and-swap ,place ,old ,new)
  #+sbcl
  (let ((tmp (gensym "OLD")))
    `(let ((,tmp ,old)) (eql ,tmp (sb-ext:cas ,place ,tmp ,new))))
  #-(or allegro ecl ccl lispworks sbcl)
  (error "Implementation not supported."))
