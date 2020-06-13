(adt:defdata (term-type :mutable t)
  "Data type for FormalityCore terms."
  ;; var indx
  ;; ref name
  ;; all eras self name bind body
  ;; lam eras name body
  ;; app eras func argm
  ;; let name expr body
  ;; ann done expr type
  ;; loc from upto expr
  (ver (integer 0))
  (ref string)
  typ
  (all boolean string string term-type function)
  (lam boolean string function)
  (app boolean term-type term-type)
  (lat string term-type function)
  (ann boolean term-type term-type))

;;; Some utility macros
;; (lam* eras (A B ...) body) expands to (lam eras "A" (lambda (A) (lam eras "B" (lambda (B) ...body...))))
(defmacro lam* (eras vars body)
  (let ((vars (reverse vars)))
    (dolist (var vars)
      (setf body
            `(lam ,eras ,(write-to-string var) (lambda (,var) ,body))))
    body))

;; (app* func a b ...) expands to ...(app eras (app eras func a) b)...
(defmacro app* (eras func arg &rest args)
  (dolist (x (cons arg args))
    (setf func `(app ,eras ,func ,x)))
  func)

(defmacro lat* (name expr body)
  `(lat
    ,(write-to-string name)
    ,expr
    (lambda (,name) ,body)))

(defmacro all* (eras self name expr body)
  `(all
    ,eras
    ,(write-to-string self)
    ,(write-to-string name)
    ,expr
    (lambda (,self ,name) ,body)))

;; Function that transforms simple CL lambda terms to FormalityCore terms
(defun cl->term (qcode)
  (trivia:match qcode
    ((cons 'lambda rest)
     (trivia:match rest
       ((list args body) (macroexpand `(lam* nil ,args ,(cl->term body))))
       (_ (error "Improper lambda expression."))))
    ((cons _ _)
     (macroexpand `(app* nil ,@(mapcar #'cl->term qcode))))
    (_ (cond
         ((typep qcode 'integer) (ver qcode))
         ((typep qcode 'symbol) qcode)
         (t (error "Cannot be translated to FormalityCore term."))))))

;; Macro version of last function (runs at compile time)
(defmacro cl->term-macro (code) (cl->term code))

(defun whnf (term defs &optional erased)
  (adt:match term-type term
    ((ref name)
     (if (gethash name defs)
         (whnf (gethash name defs) defs erased)
         (error (format nil "~A is undefined." name))))
    ((lam eras name body)
     (if (and erased eras)
         (whnf (funcall body (lam nil "" (lambda (x) x))) defs erased)
         (lam eras name body)))
    ((app eras func argm)
     (if (and erased eras)
         (whnf func defs erased)
         (let ((func (whnf func defs erased)))
           (adt:match term-type func
             ((lam _ _ body)
              (whnf (funcall body argm) defs erased))
             (_
              (app eras func argm))))))
    ((lat _ expr body) (whnf (funcall body expr) defs erased))
    ((ann _ expr _) (whnf expr defs erased))
    (_ term)))
