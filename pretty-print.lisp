(in-package #:formality)

(defun stringify (term)
  (adt:match term-type term
             ((ver indx) (format nil "#~A" indx))
             ((ref name) (write-to-string name))
             (typ "*")
             ((all eras self name bind body)
              (format nil "~A~A(~A: ~A) ~A"
                      (if eras "∀" "Π")
                      (if (equal self "_") "" self)
                      name
                      (stringify bind)
                      (stringify (funcall body (ref (read-from-string self)) (ref (read-from-string name))))))
             ((lam eras name body)
              (format nil "~A~A ~A"
                      (if eras "Λ" "λ")
                      name
                      (stringify (funcall body (ref (read-from-string name))))))
             ((app eras func argm)
              (if eras
                  (format nil "<~A ~A>"
                          (stringify func)
                          (stringify argm))
                  (format nil "(~A ~A)"
                          (stringify func)
                          (stringify argm))))
             ((lat name expr body)
              (format nil "$ ~A = ~A; ~A"
                      name
                      (stringify expr)
                      (stringify (funcall body (ref (read-from-string name))))))
             ((ann done expr type)
              (format nil ":~A ~A"
                      (stringify type)
                      (stringify expr)))))
