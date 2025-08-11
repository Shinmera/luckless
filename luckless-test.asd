(asdf:defsystem luckless-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test suite for Luckless"
  :homepage "https://shinmera.com/docs/luckless/"
  :bug-tracker "https://shinmera.com/project/luckless/issues"
  :source-control (:git "https://shinmera.com/project/luckless.git")
  :serial T
  :components ((:file "tests"))
  :depends-on (:luckless
               :parachute
               :bordeaux-threads
               :alexandria)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :luckless-test)))
