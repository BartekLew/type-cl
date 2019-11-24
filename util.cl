(defvar *included* nil)

(defmacro include (file)
	`(cond ((not (find ,file *included* :test #'string=))
              (load (merge-pathnames ,file *load-truename*))
              (setf *included* (cons ,file *included*))
              T)))

(defmacro test (form result test)
  `(handler-case
     (let ((ans ,form))
       (if (not (,test ans ,result))
         (format t "TEST FAILED: ~S != ~S~%" ans ,result)))
     (error (e) (format t "TEST FALED ~S thrown ~S~%" ',form (type-of e)))))

(defmacro test-error (form condition)
  `(handler-case
     (let ((ans ,form))
        (format t "TEST FAILED: no error (~A), rather ~S~%" ',condition ans))
     (,condition () T)
     (condition (e) (format t "TEST FAILED: wrong condition ~A != ~A.~%"
                            (type-of e) ',condition))))

(defun id(x) x)

(defun listp+ (l)
  (and l (listp l)))

(defmacro f (args &body body)
  `(labels ((self ,args ,@body))
     (lambda (&rest args)
       (apply #'self args))))

(defun f+ (&rest functions)
  (if (not functions) #'id
      (let ((revf (reverse (remove #'id functions))))
        (if (= (length revf) 1) (first revf)
          (lambda (&rest args)
            (let ((result (apply (first revf) args)))
              (loop for f in (rest revf)
                    do (setf result (apply f (list result))))
              result))))))
