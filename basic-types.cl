(include "categories.cl")

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

