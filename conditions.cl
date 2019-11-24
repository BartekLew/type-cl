(define-condition call-type-mismatch (error)
  ((form :initarg := :reader form)))

(defmethod print-object ((this call-type-mismatch) out)
  (format nil "No suitable call for form: ~A" (form this)))

