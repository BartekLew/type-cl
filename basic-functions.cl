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

(labels ((match-types (expected args)
           (cond ((or (not expected) (eql expected 'List))
                    (match-same-type-args 'Any args))
                 ((eql (first expected) 'List)
                         (match-same-type-args (second expected) args))
               (t (error 'call-type-mismatch := args)))))
    (setf (fn 'list #'match-types)
         (lambda (&rest args)
                 (apply #'list args))))


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

