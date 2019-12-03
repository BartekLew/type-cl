(define-condition call-type-mismatch (error)
  ((form :initarg := :reader form)))

(defun unknown-function-error (name)
  (error 'call-type-mismatch := `(fn ,name)))

(defmethod print-object ((this call-type-mismatch) out)
  (format nil "No suitable call for form: ~A" (form this)))

