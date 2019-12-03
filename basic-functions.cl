(include "categories.cl")
(include "basic-types.cl")

(defun match-arg (expected arg)
    (cond ((listp+ arg)
             (!! arg expected))
          ((check expected arg)
             arg)
          (t (apply (or (converter (detect-type arg) expected)
                        (error 'call-type-mismatch))
                    (list arg)))))

(defun match-same-type-args (expected args)
    (loop for arg in args
          collect (match-arg expected arg)))

(defun simple-vararg (intype outtype)
  (lambda (expected args)
      (if (and expected (not (equalp outtype expected)))
        (error 'call-type-mismatch := `(,expected ,args)))
      (match-same-type-args intype args)))

(defun deduce (template val)
  (if (not val) (return-from deduce 'Any))
  (if (eql template '_) (return-from deduce val))
  (if (not (and (listp+ template) (listp+ val)))
    (error 'call-type-mismatch := `(,template ,val)))
  (let (ans)
    (loop for a in template
          for b in val
          do (if (eql a '_) (setf ans b)
               (if (not (equalp a b))
                 (error 'call-type-mismatch := `(,template ,val)))))
    ans))

(defun puttype (template value)
  (if (eql template '_) value
  (if (not (listp template)) template
    (loop for x in template
          collect (if (eql x '_) value x)))))

(defun par-vararg (typespec)
  (lambda (expected args &optional mode)
    (let ((par (deduce (car (last typespec)) expected)))
      (if (not (= (length args) (- (length typespec) 1)))
        (error 'call-type-mismatch := `(,typespec ,expected ,args)))
      (loop for arg in args
                for ref in typespec
                collect (or (let ((typ (puttype ref par)))
                              (if (eql mode :typespec) (and (eql typ arg) typ) (match-arg typ arg)))
                            (error 'call-type-mismatch := `(,typespec ,expected ,args)))))))

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

(setf (fn '+ '(Int (Function Int Int)))
      (lambda (x)
        (lambda (y)
          (+ x y))))

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

(setf (fn 'map (par-vararg '((Function _ _) (List _) (List _))))
      (lambda (f l)
        (loop for x in l
              collect (if (functionp f) (apply f (list x))
                        (!! `(,f ,x))))))

(setf (fn 'fold (par-vararg '((Function _ _ _) (List _) _)))
      (lambda (f l)
        (let ((ans (first l)))
          (loop for x in (rest l)
                do (setf ans
                         (if (functionp f)
                           (apply f (list ans x))
                           (!! `(,f ,ans ,x)))))
          ans)))
