(load (merge-pathnames "type.cl" *load-truename*))

(test (!! '(+ "foo " "bar"))
                "foo bar" string=)
(test (!! '(+ "doo " 2))
                "doo 2" string=)

(test (!! '(+ "doo " nil)) "doo NIL" string=)

(test (!! '(+ 2 2))
             4 =)
(test (!! '(- 2 2))
             0 =)
(test (!! '(* 3 2))
             6 =)
(test (!! '(/ 3 2))
             3/2 =)
(test (!! '(+ (list 3 2 7)))
             12 =)
(test (!! '(* (list 5 5 10)))
             250 =)
(test-error (!! '(* (list "foo" 5 10)))
             call-type-mismatch)

(test (!! '(list 1)) (list 1) equalp)

(test (!! '(list 1 2 3 4 5)) (list 1 2 3 4 5) equalp)

(test (!! '(list)) (list) equalp)

(test (!! '(* (+ 2 2) (- 4 2))) 8 =)

(test (!! '(avg (list 5 10 12)))
      9 =)

(test (apply (converter 'Symbol 'String) '(foo)) "FOO" string=)

(flet ((p5 (x) (+ 5 x))
       (m2 (x) (* 2 x)))
  (test (apply (f+ #'m2 #'p5) (list 11)) 32 =))

(test (eql (f+ #'id #'+) #'+) T eql)

(test (detect-type "Foo") 'String eql)

(!! '(+ foo "-bar"))
