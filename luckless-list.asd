#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem luckless-list
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A lockless linked list implementation"
  :homepage "https://shinmera.github.io/luckless/"
  :bug-tracker "https://github.com/Shinmera/luckless/issues"
  :source-control (:git "https://github.com/Shinmera/luckless.git")
  :serial T
  :components ((:file "list"))
  :depends-on ())
