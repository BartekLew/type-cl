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

(defun listp+ (l)
  (and l (listp l)))

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
                  (gethash name fns)))
    (defmethod print-object ((this (eql def)) out)
        (format out "#<~A ~A>" name type-spec))))

(let ((types (make-hash-table)))
  (defun check (name value)
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

(setf (check 'Any) (lambda (x) (declare (ignore x)) T))
(setf (check 'Int) #'integerp)
(setf (check 'String) #'stringp)
(setf (check 'Number) #'numberp)
(setf (check 'List)
      (lambda (l &optional etype)
        (and (listp+ l)
             (or (not etype)
                 (not (position-if (lambda (x) (not (check etype x))) l))))))

(define-condition call-type-mismatch (error)
  ((form :initarg := :reader form)))

(defmethod print-object ((this call-type-mismatch) out)
  (format nil "No suitable call for form: ~A" (form this)))

(defun type-match (form &optional rettype)
  (let ((fargs (rest form))
        (specs (fn (first form))))
    (flet ((exec (spec)
             (let ((types (first spec))
                   (fun (second spec)))
               (if (and rettype (not (equalp rettype (car (last types)))))
                 (error 'call-type-mismatch := form))
               (funcall #'apply fun
                     (loop for arg in fargs
                           for typ in types
                           collect (cond ((listp+ arg) (type-match arg typ))
                                         ((check typ arg) arg)
                                         (T (error 'call-type-mismatch := form))))))))
      (loop for spec in specs
            do (handler-case
                 (return-from type-match (exec spec))
                 (call-type-mismatch () nil)))
      (error 'call-type-mismatch := form))))

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

(setf (fn 'list '(Number Number Number (List Number)))
      (lambda (&rest args)
             (apply #'list args)))

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

(test (type-match '(+ "foo " "bar"))
                "foo bar" string=)
(test (type-match '(+ "doo " 2))
                "doo 2" string=)

(test-error (type-match '(+ "doo " nil))
                call-type-mismatch)
(test (type-match '(+ 2 2))
             4 =)
(test (type-match '(- 2 2))
             0 =)
(test (type-match '(* 3 2))
             6 =)
(test (type-match '(/ 3 2))
             3/2 =)
(test (type-match '(+ (list 3 2 7)))
             12 =)
(test (type-match '(* (list 5 5 10)))
             250 =)
(test-error (type-match '(* (list "foo" 5 10)))
             call-type-mismatch)

(test (type-match '(* (+ 2 2) (- 4 2))) 8 =)

(setf (fn 'avg '((List Number) Number))
      (lambda (x)
        (type-match `(/ (+ (list ,@x)) ,(length x)))))

(test (type-match '(avg (list 5 10 12)))
      9 =)
