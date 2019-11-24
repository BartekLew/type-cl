(load (merge-pathnames "util.cl" *load-truename*))

(let ((fns (make-hash-table)))
  (defun fn (name)
    (or (gethash name fns)
        (error "Function not found: ~A." name)))
  (defun (setf fn) (def name type-spec)
    (loop for d in (gethash name fns)
          do (if (equalp (first d) type-spec)
               (error "Function already defined ~A : ~A~%"
                      name type-spec)))
    (setf (gethash name fns)
            (cons (list type-spec def)
                  (gethash name fns)))
    (defmethod print-object ((this (eql def)) out)
        (format out "#<~A ~A>" name type-spec))))

(let ((types (make-hash-table)))
  (defun check (name value)
    (if (not name) (return-from check value))
    (let ((checker (gethash (if (listp+ name)
                               (first name) 
                               name)
                            types)))
      (if (not checker) (error "Type checker not found: ~A." name))
      (apply checker (if (listp+ name)
                        (cons value (rest name))
                        (list value)))))
  (defun (setf check) (fn name)
    (if (gethash name types) (error "Type checker already defined: ~A" name))
    (setf (gethash name types) fn)))

(let ((casts (make-hash-table :test #'equalp)))
  (defun converter (from to)
    (let ((act (gethash `(,from ,to) casts)))
      (if (not act) (error "conversion not found: ~A -> ~A." from to))
      act))
  (defun (setf converter) (action from to)
    (if (gethash `(,from ,to) casts)
       (error "conversion already defined: A~ -> ~A" from to))
    (setf (gethash `(,from ,to) casts) action)))

(setf (check 'Any) (lambda (x) (declare (ignore x)) T))
(setf (check 'Int) #'integerp)
(setf (check 'String) #'stringp)
(setf (check 'Char) #'characterp)
(setf (check 'Symbol) #'symbolp)
(setf (check 'Number) #'numberp)
(setf (check 'List)
      (lambda (l &optional etype)
        (and (listp+ l)
             (or (not etype)
                 (not (position-if (lambda (x) (not (check etype x))) l))))))

(setf (converter 'Symbol 'String)
      (lambda (x) (format nil "~A" x)))

(defun defprod (name)
  (setf (check name)
        (lambda (val)
          (declare (ignore val))
          (error 'call-type-mismatch := name))))

(define-condition call-type-mismatch (error)
  ((form :initarg := :reader form)))

(defmethod print-object ((this call-type-mismatch) out)
  (format nil "No suitable call for form: ~A" (form this)))

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

