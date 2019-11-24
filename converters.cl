(include "categories.cl")

(setf (converter 'Symbol 'String)
      (lambda (x) (format nil "~A" x)))

