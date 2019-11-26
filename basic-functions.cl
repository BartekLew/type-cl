(include "categories.cl")
(include "basic-types.cl")

(defun match-same-type-args (expected args)
    (loop for arg in args
          collect (cond ((listp+ arg)
                           (!! arg expected))
                        ((check expected arg)
                           arg)
                        (t (apply (or (converter (detect-type arg) expected)
                                      (error 'call-type-mismatch))
                                  (list arg))))))

(defun simple-vararg (intype outtype)
  (lambda (expected args)
      (if (and expected (not (equalp outtype expected)))
        (error 'call-type-mismatch := `(,expected ,args)))
      (match-same-type-args intype args)))

(defun deduce (template val)
  (if (not val) (return-from deduce 'Any))
  (if (not (and (listp+ template) (listp+ val)))
    (error 'call-type-mismatch := `(,template ,val)))
  (let (ans)
    (loop for a in template
          for b in val
          do (if (eql a '_) (setf ans b)
               (if (not (equalp a b))
                 (error 'call-type-mismatch := `(,template ,val)))))
    ans))

(defun simple-gen-vararg (outtype)
  (lambda (expected args)
    (let ((intype (deduce outtype expected)))
      (match-same-type-args intype args))))

(setf (fn 'list (simple-gen-vararg '(List _)))
    (lambda (&rest args)
       (apply #'list args)))


(setf (fn '+ (simple-vararg 'String 'String))
      (lambda (&rest args)
        (apply #'concatenate (cons 'string args))))

(setf (fn '+ '(String Int String))
      (lambda (a b)
        (format nil "~A~A" a b)))

(loop for op in '(+ - * /)
      do (eval `(progn
                  (setf (fn ',op '(Int Int Int))
                      (lambda (a b)
                        (,op a b)))
                  (setf (fn ',op '(Number Number Number))
                      (lambda (a b)
                        (,op a b))))))
               
(loop for op in '(+ -)
      do (eval `(progn
                  (setf (fn ',op '((List Number) Number))
                      (f (x &optional (acc 0))
                        (if (not x) acc
                          (self (rest x) (,op acc (first x)))))))))

(loop for op in '(* /)
      do (eval `(progn
                  (setf (fn ',op '((List Number) Number))
                      (f (x &optional (acc 1))
                        (if (not x) acc
                          (self (rest x) (,op acc (first x)))))))))

(setf (fn 'avg '((List Number) Number))
      (lambda (x)
        (!! `(/ (+ (list ,@x)) ,(length x)))))

