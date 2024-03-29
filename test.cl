(load (merge-pathnames "type.cl" *load-truename*))

(test (!! '(+ "foo " "bar"))
                "foo bar" string=)

(test (!! '(+ "doo " 2))
                "doo 2" string=)

(test-error (!! '(+ "doo " nil)) call-type-mismatch)

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

(test (!! '(+ foo "-bar")) "FOO-bar" string=)

(test (format nil "~A" (check '(Function String Int String) '+))
      "#<+ (STRING INT STRING)>" string=)

(test (apply (!! '(+ 1)) '(3)) 4 eql)

(test (!! '(map (+ 2) (list 7 8 9)) '(List Int)) '(9 10 11) equalp)
(test (!! '(fold + (list 5 6 8)) 'Int) 19 =)

(setf (fn 'logstr '(String String))
      (lambda (x)
        (format nil "++ ~A" x)))

(test (!! '(logstr "a")) "++ a" string=)

(test (apply (par-vararg '(_ _ _)) '(Int (1 2))) '(1 2) equalp)
(test (apply (par-vararg '(_ _ _)) '(String ("foo" "bar"))) '("foo" "bar") equalp)
(test (apply (par-vararg '((List _) _)) '(String ((list "foo" "bar")))) '(("foo" "bar")) equalp)
(test (apply (par-vararg '(_ _ _)) '(String (String String) :typespec)) '(String String) equalp)
(test (apply (par-vararg '(_ _ (List _))) '((List String) (String String) :typespec)) '(String String) equalp)

(test (!! '(map logstr (list "foo" "bar" "baz")) '(List String)) '("++ foo" "++ bar" "++ baz") equalp)
(test (!! '(fold + (list "foo-" "bar-" "baz")) 'String) "foo-bar-baz" string=)

(test (apply (simple-vararg 'Symbol '(List Symbol))
              '((List Symbol) (foo bar baz))) '(foo bar baz) equalp)

(test (apply (simple-vararg 'Int '(List Int))
              '((List Int) (1 2 (+ 1 2)))) '(1 2 3) equalp)

(test (apply (simple-gen-vararg '(List _))
             '((List Symbol) (a b c))) '(a b c) equalp)

(test-error (apply (simple-gen-vararg '(List _))
                '(Number (1 2 3)))
            call-type-mismatch)
