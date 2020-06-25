(in-package #:formality)

(adt:defdata erasure
    (erase t))

(defun single-erase-reader (stream char)
  (declare (ignore char))
  (list (quote erase) (read stream t nil t)))
(set-macro-character #\! #'single-erase-reader)

(defun extract-eras (x) (adt:match erasure x ((erase x) x)))

;; (lam* eras (A B ...) body) expands to (lam eras "A" (lambda (A) (lam eras "B" (lambda (B) ...body...))))
(defmacro lam* (vars body)
  (dolist (var (reverse vars))
    (setf body
          (let ((eras (typep var 'erasure)))
            (when eras (setf var (extract-eras var)))
            `(lam ,eras ,(write-to-string var) (lambda (,var) ,body)))))
  body)

;; (app* func a b ...) expands to ...(app eras (app eras func a) b)...
(defmacro app* (func arg &rest args)
  (dolist (arg (cons arg args))
    (setf func
          (let ((eras (typep arg 'erasure)))
            (when eras (setf arg (extract-eras arg)))
            `(app ,eras ,func ,arg))))
  func)

(defmacro lat* (((name expr) &rest binds) body)
  (dolist (bind (reverse (cons (list name expr) binds)))
    (trivia:match bind
      ((list name expr)
       (setf body
             `(lat ,(write-to-string name) ,expr (lambda (,name) ,body))))
      (_ (error "Malformed let binding."))))
  body)

(defmacro all* (self name bind body)
  (let ((eras (typep bind 'erasure)))
    (when eras (setf bind (extract-eras bind)))
    `(all
      ,eras
      ,(write-to-string self)
      ,(write-to-string name)
      ,bind
      (lambda (,self ,name) ,body))))

;; Function that transforms simple CL lambda terms to FormalityCore terms
(defun cl->term (qcode &optional (names (make-hash-table)))
  (setf names (alexandria:copy-hash-table names)) ;; to avoid side-effects
  (trivia:match qcode
    ((list 'lambda args body)
     (let ((traverse (lambda (arg)
                       (trivia:match arg
                         ((list 'erase arg)
                          (unless (symbolp arg)
                            (error "Lambda arguments must be either symbols or erased symbols."))
                          (setf (gethash arg names) t) (erase arg))
                         ((type symbol) (setf (gethash arg names) t) arg)
                         (_ (error "Lambda arguments must be either symbols or erased symbols."))))))
       (macroexpand `(lam* ,(mapcar traverse args) ,(cl->term body names)))))
    ((cons 'lambda _) (error "Improper lambda expression."))
    ((list 'let binds body)
     (macroexpand
      `(lat* ,(mapcar
               (lambda (bind)
                 (trivia:match bind
                   ((list name expr)
                    ;; Since let is not recursive, setf should come afterwards
                    (let ((res (list name (cl->term expr names))))
                      (setf (gethash name names) t)
                      res))
                   (_ (error "Malformed let binding."))))
               binds)
             ,(cl->term body names))))
    ((cons 'let _) (error "Improper let expression."))
    ((list 'erase x) (erase (cl->term x names)))
    ((list 'all self (list name bind) body)
     (macroexpand
      (let* ((bind (cl->term bind names))
             (ename (trivia:match name
                      ((list 'erase ename)
                       (unless (symbolp ename)
                         (error "All arguments must be either symbols or erased symbols."))
                       (setf name ename)
                       (erase ename))
                      ((type symbol) name)
                      (_ (error "All arguments must be either symbols or erased symbols."))))
             (body (progn
                     (setf (gethash self names) t (gethash name names) t)
                     (cl->term body names))))
        `(all* ,self ,name ,bind ,body))))
    ((cons 'all _) (error "Improper all expression."))
    ((list x '@ y) `(ann nil ,(cl->term x names) ,(cl->term y names)))
    ((cons x _)
     (macroexpand `(app* ,@(mapcar (lambda (qcode) (cl->term qcode names)) qcode))))
    ((type integer) `(ver ,qcode))
    ('* 'typ)
    ((or 'lambda 'let 'all 'erase '@) (error (format nil "~A is a keyword." qcode)))
    ((type symbol)
     (if (gethash qcode names) qcode `(ref (quote ,qcode))))
    (_ (error "Cannot be translated to FormalityCore term."))))

;; Macro version of last function (runs at compile time)
(defmacro cl->term-macro (code) (cl->term code))
