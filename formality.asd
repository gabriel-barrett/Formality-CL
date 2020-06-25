(asdf:defsystem #:formality
  :description "Formality implementation in Common Lisp."
  :version "0.0.6"
  :author "Gabriel Barreto <gabriel.aquino.barreto@gmail.com>"
  :license "GPLv3"
  :depends-on (#:alexandria #:cl-algebraic-data-type #:trivia)
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:file "package")
               (:file "core")
               (:file "cl-to-fm")
               (:file "pretty-print")))
