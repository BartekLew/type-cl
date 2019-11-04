(defmacro test (form result test)
  `(let ((ans ,form))
     (if (not (,test ans ,result))
       (format t "TEST FAILED: ~S != ~S~%" ans ,result))))

(defmacro test-error (form condition)
  `(handler-case
     (let ((ans ,form))
        (format t "TEST FAILED: no error (~A), rather ~S~%" ',condition ans))
     (,condition () T)
     (condition (e) (format t "TEST FAILED: wrong condition ~A != ~A.~%"
                            (type-of e) ',condition))))

(defmacro f (args &body body)
  `(labels ((self ,args ,@body))
     (lambda (&rest args)
       (apply #'self args))))

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
                  (gethash name fns)))))

(let ((types (make-hash-table)))
  (defun check (name value)
    (let ((checker (gethash name types)))
      (if (not checker) (error "Type checker not found: ~A." name))
      (apply checker (list value))))
  (defun (setf check) (fn name)
    (if (gethash name types) (error "Type checker already defined: ~A" name))
    (setf (gethash name types) fn)))

(setf (check 'Int) #'integerp)
(setf (check 'String) #'stringp)
(setf (check 'Number) #'numberp)
(setf (check 'List) #'listp)

(defun arg-match? (args type-spec)
  (cond ((= (length args) (- (length type-spec) 1))
            (loop for i from 0 to (- (length args) 1)
                  do (if (not (check (nth i type-spec) (nth i args)))
                       (return-from arg-match? nil)))
            (return-from arg-match? (car (last type-spec))))))

(define-condition call-type-mismatch (error)
  ((form :initarg := :reader form)))

(defmethod print-object ((this call-type-mismatch) out)
  (format nil "No suitable call for form: ~A" (form this)))

(defun appfn (form)
  (let ((name (first form))
        (args (rest form)))
    (loop for def in (fn name)
          do (let ((rett (arg-match? args (first def))))
                (if rett (return-from appfn (values (apply (second def) args)
                                                    rett)))))
    (error 'call-type-mismatch := form)))

(setf (fn '+ '(String String String))
      (lambda (a b)
        (concatenate 'string a b)))

(setf (fn '+ '(String Int String))
      (lambda (a b)
        (format nil "~A~A" a b)))


(loop for op in '(+ - * /)
      do (eval `(progn
                  (setf (fn ',op '(Number Number Number))
                      (lambda (a b)
                        (,op a b))))))
               
(loop for op in '(+ -)
      do (eval `(progn
                  (setf (fn ',op '(List Number))
                      (f (x &optional (acc 0))
                        (if (not x) acc
                          (self (rest x) (,op acc (first x)))))))))

(loop for op in '(* /)
      do (eval `(progn
                  (setf (fn ',op '(List Number))
                      (f (x &optional (acc 1))
                        (if (not x) acc
                          (self (rest x) (,op acc (first x)))))))))

               
(test (appfn '(+ "foo " "bar"))
                "foo bar" string=)
(test (appfn '(+ "doo " 2))
                "doo 2" string=)
(test-error (appfn '(+ "doo " nil))
                call-type-mismatch)
(test (appfn '(+ 2 2))
             4 =)
(test (appfn '(- 2 2))
             0 =)
(test (appfn '(* 3 2))
             6 =)
(test (appfn '(/ 3 2))
             3/2 =)
(test (appfn '(+ (3 2 7)))
             12 =)
(test (appfn '(* (5 5 10)))
             250 =)
