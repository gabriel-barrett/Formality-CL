(asdf:defsystem #:formality
  :description "Formality implementation in Common Lisp"
  :version "0.0.2"
  :author "Gabriel Barreto <gabriel.aquino.barreto@gmail.com>"
  :license "GPLv3"
  :depends-on (#:cl-algebraic-data-type #:trivia)
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:file "formalitycore")))
