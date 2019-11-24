(include "categories.cl")
(include "basic-types.cl")

(setf (fn '+ '(String String String))
      (lambda (a b)
        (concatenate 'string a b)))

(setf (fn '+ '(String Int String))
      (lambda (a b)
        (format nil "~A~A" a b)))

(labels ((match-arg (expected args)
           (loop for arg in args
                 collect (cond ((listp+ arg)
                                 (!! arg expected))
                               ((check expected arg)
                                 arg)
                               (t (error 'call-type-mismatch := args)))))
         (match-types (expected args)
           (cond ((or (not expected) (eql expected 'List))
                    (match-arg 'Any args))
                 ((eql (first expected) 'List)
                         (match-arg (second expected) args))
               (t (error 'call-type-mismatch := args)))))
    (setf (fn 'list #'match-types)
          (lambda (&rest args)
                 (apply #'list args))))

(loop for op in '(+ - * /)
      do (eval `(progn
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

