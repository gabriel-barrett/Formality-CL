(defpackage #:formality
  (:documentation "Formality implementation in Common Lisp.")
  (:use #:cl)
  (:nicknames #:fm)
  (:export
   #:cl->term-macro                     ; MACRO
   #:term-type                          ; TYPE
   #:term-type-p                        ; FUNCTION (PREDICATE)
   #:cl->term                           ; FUNCTION
   #:norm                               ; FUNCTION
   #:whnf                               ; FUNCTION
   #:equal-term                         ; FUNCTION
   #:stringify                          ; FUNCTION
   ))
