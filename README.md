type-cl — static dependent types in Common Lisp
===============================================

This project is going to implement static, compile-time-able
dependent type system in Common Lisp. Currently only some runtime
features are implemented. See `test.cl` to see what functions
are implemented.

Dependent types
---------------

It's quite a niche idea, called Type Driven Development in
which, basically, types are lifted to be first-class object,
just like functions are data. This means more freedom in
defining types. This means that we can define types with
parameters, like `List 5` meaning list of size 5. You can
do it in OOP, but only with runtime checking. Moreover,
with dependent types you can express that function append
provide list having size of sum of lengths input lists.

There can be virtually any assumption put into type definition,
being able to check it in compile-time wherever possible. It
let's to get much better guarantees about code validity then
using tests.

Liberal static typing
---------------------

To get maximum control, type-cl provides static typing, because
it lets detect possible errors in compile time (dynamic typing lets
you know only in case when error happen). On the other hand, type-cl
is going to put flexibility to reasonable limits. It means, you can
define the same function not only for various parameter lists, but
also return types, meaning I can have at the same time:

```
append : String -> String -> String
append : String -> String -> List
```

Right function will be chosen depending on type expected in given place.
As many combinations may be expressed in code, type-cl is going to be
able to consider all of them to cover as many cases as possible. Also
it's going to deduce run-time assertions. For example we have function
that accept line of text, but a String would be provided. The warning
would be issued and proper runtime type check done before apply that
argument.

There is going to be implicit conversion mechanism allowing to convert
automatically one type to the other, for example integer to string.
Conversion would be treated as backup option, compiler would try to do
as few implicit conversions and runtime checks as possible.
