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
  (ver integer)
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
  (dolist (var (reverse vars))
    (setf body
          `(lam ,eras ,(write-to-string var) (lambda (,var) ,body))))
  body)

;; (app* func a b ...) expands to ...(app eras (app eras func a) b)...
(defmacro app* (eras func arg &rest args)
  (dolist (arg (cons arg args))
    (setf func
          `(app ,eras ,func ,arg)))
  func)

(defmacro lat* (((name expr) &rest binds) body)
  (dolist (bind (reverse (cons (list name expr) binds)))
    (trivia:match bind
      ((list name expr)
       (setf body
             `(lat ,(write-to-string name) ,expr (lambda (,name) ,body))))
      (_ (error "Malformed let binding."))))
  body)

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
    ((cons 'let rest)
     (trivia:match rest
       ((list binds body)
        (macroexpand
         `(lat* ,(mapcar
                  #'(lambda (bind)
                      (trivia:match bind
                        ((list name expr) (list name (cl->term expr)))
                        (_ (error "Malformed let binding."))))
                  binds)
                ,(cl->term body))))
       (_ (error "Improper let expression."))))
    ((cons _ _)
     (macroexpand `(app* nil ,@(mapcar #'cl->term qcode))))
    ((type integer) (ver qcode))
    ('* 'typ)
    ((or 'lambda 'let) (error (format nil "~A is a keyword." qcode)))
    ((type symbol) qcode)
    (_ (error "Cannot be translated to FormalityCore term."))))

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

(defun hash (term &optional (dep 0))
  (adt:match term-type term
    ((ver indx)
     (if (< indx 0)
         (format nil "^~d" (+ dep indx))
         (format nil "#~d" indx)))
    ((ref name)
     (concatenate 'string "$" name))
    (typ
     "type")
    ((all _ self _ bind body)
     (let ((bind (hash bind dep))
           (body (hash (funcall body (ver (- -1 dep)) (ver (- -2 dep))) (+ 2 dep))))
       (concatenate 'string "π" self bind body)))
    ((lam _ _ body)
     (let ((body (hash (funcall body (ver (- -1 dep))) (+ 1 dep))))
       (concatenate 'string "λ" body)))
    ((app _ func argm)
     (let ((func (hash func dep))
           (argm (hash argm dep)))
       (concatenate 'string "@" func argm)))
    ((lat _ expr body)
     (let ((expr (hash expr dep))
           (body (hash (funcall body (ver (- -1 dep))) (+ 1 dep))))
       (concatenate 'string "$" expr body)))
    ((ann _ expr _)
     (hash expr dep))))

(defun norm (term defs &optional erased (seen (make-hash-table :test 'equal)))
  (let* ((head (whnf term defs erased))
         (term_hash (hash term))
         (head_hash (hash head)))
    (if (or (gethash term_hash seen) (gethash head_hash seen)) term
        (progn
          (setf (gethash term_hash seen) t (gethash head_hash seen) t)
          (adt:match term-type head
            ((all eras self name bind body)
             (let ((bind (norm bind defs erased seen))
                   (body (lambda (s x) (norm (funcall body s x) defs erased seen))))
               (all eras self name bind body)))
            ((lam eras name body)
             (let ((body (lambda (x) (norm (funcall body x) defs erased seen))))
               (lam eras name body)))
            ((app eras func argm)
             (let ((func (norm func defs erased seen))
                   (argm (norm argm defs erased seen)))
               (app eras func argm)))
            (_ head))))))
