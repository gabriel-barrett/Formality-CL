(in-package #:formality)

(adt:defdata term-type
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
  (ref symbol)
  typ
  (all boolean string string term-type function)
  (lam boolean string function)
  (app boolean term-type term-type)
  (lat string term-type function)
  (ann boolean term-type term-type))

;; (defun term-type-p

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
     (format nil "$~A" name))
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

(defun norm (term defs &optional erased (seen (make-hash-table)))
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

;; Are two terms equal?
(defun equal-term (a b defs &optional (dep 0) (seen (make-hash-table :test 'equal)))
  (let* ((a1 (whnf a defs t))
         (b1 (whnf b defs t))
         (ah (hash a1))
         (bh (hash b1))
         (id (format nil "~A==~A" ah bh)))
    (when (or (equal ah bh) (gethash id seen)) (return-from equal-term t))
    (setf (gethash id seen) t)
    (adt:match term-type a1
      ((all a1-eras a1-self a1-name a1-bind a1-body)
       (adt:match term-type b1
         ((all b1-eras b1-self b1-name b1-bind b1-body)
          (setf a1-body (funcall a1-body (ver dep) (ver (+ dep 1))))
          (setf b1-body (funcall b1-body (ver dep) (ver (+ dep 1))))
          (and
           (equal a1-eras b1-eras)
           (equal a1-self b1-self)
           (equal-term a1-bind b1-bind defs dep seen)
           (equal-term a1-body b1-body defs (+ 2 dep) seen)))
         (_ nil)))
      ((lam a1-eras a1-name a1-body)
       (adt:match term-type b1
         ((lam b1-eras b1-name b1-body)
          (setf a1-body (funcall a1-body (ver dep)))
          (setf b1-body (funcall b1-body (ver dep)))
          (and
           (equal a1-eras b1-eras)
           (equal-term a1-body b1-body defs (+ 1 dep) seen)))
         (_ nil)))
      ((app a1-eras a1-func a1-argm)
       (adt:match term-type b1
         ((app b1-eras b1-func b1-argm)
          (and
           (equal a1-eras b1-eras)
           (equal-term a1-func b1-func defs dep seen)
           (equal-term a1-argm b1-argm defs dep seen)))
         (_ nil)))
      ((lat a1-name a1-expr a1-body)
       (adt:match term-type b1
         ((lat b1-name b1-expr b1-body)
          (setf a1-body (funcall a1-body (ver dep)))
          (setf b1-body (funcall b1-body (ver dep)))
          (and
           (equal-term a1-expr b1-expr defs dep seen)
           (equal-term a1-body b1-body defs (+ 1 dep) seen)))
         (_ nil)))
      (_ nil))))
