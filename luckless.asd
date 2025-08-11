(asdf:defsystem luckless
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Lockless data structures"
  :homepage "https://shinmera.com/docs/luckless/"
  :bug-tracker "https://shinmera.com/project/luckless/issues"
  :source-control (:git "https://shinmera.com/project/luckless.git")
  :serial T
  :components ((:file "package")
               (:file "list")
               (:file "cat")
               (:file "hashtable")
               (:file "queue")
               (:file "documentation"))
  :depends-on (:atomics
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :luckless-test))))
