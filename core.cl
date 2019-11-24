(include "basic-types.cl")
(include "conditions.cl")

(defun !! (form &optional rettype)
  (flet ((assert-type (type val)
           (if (check type val) val
                 (error 'call-type-mismatch := form))))
  (let ((fargs (rest form))
        (specs (fn (first form))))
    (flet ((exec (spec)
             (let ((types (first spec))
                   (fun (second spec)))
               (cond ((listp+ types)
                        (if (/= (- (length types) (length fargs) 1) 0)
                          (error 'call-type-mismatch := form))
                        (assert-type rettype
                            (funcall #'apply fun
                                (loop for arg in fargs
                                      for typ in types
                                      collect (cond ((listp+ arg) (!! arg typ))
                                                     ((check typ arg) arg)
                                                     (T (error 'call-type-mismatch := form)))))))
                     ((functionp types)
                          (funcall types rettype fargs))
                     (t (error 'call-type-mismatch := fargs))))))
      (loop for spec in specs
            do (handler-case
                 (return-from !! (exec spec))
                 (call-type-mismatch () nil)))
      (error 'call-type-mismatch := form)))))

(defun arg-match? (args type-spec)
  (cond ((= (length args) (- (length type-spec) 1))
            (loop for i from 0 to (- (length args) 1)
                  do (if (not (check (nth i type-spec) (nth i args)))
                       (return-from arg-match? nil)))
            (return-from arg-match? (car (last type-spec))))))

