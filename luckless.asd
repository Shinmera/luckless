#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem luckless
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Lockless data structures"
  :homepage "https://shinmera.github.io/luckless/"
  :bug-tracker "https://github.com/Shinmera/luckless/issues"
  :source-control (:git "https://github.com/Shinmera/luckless.git")
  :serial T
  :components ((:file "package")
               (:file "list")
               (:file "hashtable"))
  :depends-on (:atomics
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :luckless-test))))
