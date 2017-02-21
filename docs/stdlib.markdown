# Standard Library

`(= i1 ... in)`
`(< i1 ... in)`
`(> i1 ... in)`
`(<= i1 ... in)`
`(>= i1 ... in)`

These functions are used to compare a list of integers, returning `#t` or `#f` as appropriate.

`(abs n)`

Returns the absolute value of `n`.

`(and test1 ... testN)`

Returns `#t` if all test expressions evaluate to `#t`, or if there are no test expressions.
Otherwise, returns `#f`.  **NOTE:** This function differs from standard scheme.  It does not
return the value of the last test.

`(append list1 ... listN)`

Returns a new list consisting of the elements of the first list, followed by the elements of
every other list in order.  **NOTE:** This function differs from standard scheme.  It does not
take an object as its last argument.  All arguments must be lists.

`(apply proc arg1 ... rest-args)`

Call the procedure `proc` with the given arguments.  `rest-args` is the only argument that
must be present, and it must be a list.

`(begin expr1 ... exprN)`

Evaluate all expressions, returning the value of the last one.  This is mainly used to include
side effects before returning some value.

`(boolean? obj)`

Returns `#t` if `obj` is a boolean, otherwise returns `#f`.

`(boolean=? bool1 ... boolN)`

Returns `#t` if all the boolean arguments are the same.,

`(car list)`

Returns the contents of the first element of `list`, which must not be empty.

`(cdr list)`

Returns the contents of the second element of `list`, which must be not empty.

`(char? obj)`

Returns `#t` if `obj` is a character, otherwise returns `#f`.

`(char=? ch1 ... chN)`
`(char<? ch1 ... chN)`
`(char>? ch1 ... chN)`
`(char<=? ch1 ... chN)`
`(char>=? ch1 ... chN)`

These functions are used to compare a list of characters, returning `#t` or `#f` as appropriate.

`(cond (test1 expr1) ... (testN exprN) [(else expr)])`

Evaluate `test` expressions until one returns `#t`.  When that happens, return the matching `expr`.
If no `test` passes, return the else `expr` if it exists.  If there is no else expression, return
`#f`.

`(condition? obj)`

Returns `#t` if `obj` is any error object, such as that returned from the various `make-*-error`
functions.  Returns `#f` otherwise.

`(cons obj1 obj2)`

Returns new list where the first element is `obj1` and the second element is `obj2`, which
must be a list.

`(dec n)`

Returns `n` decremented by 1.  **NOTE:** This is a function unique to this implementation of
scheme.  It does not occur in the standard library.

`(define var expr)`

Assign the value of `expr` to the variable named `var`.

`(define (var formals) body)`

Create a new function named `var` with one or more formal parameters, with the body `body`.
Names may not be reused in a single definition.

`(define (var . formal) body)`

Create a new function named `var` with a single formal parameter, with the body `body`.  All
arguments are converted into a list and passed as the single parameter.  Names may not be
reused in a single definition.

`(define (var formals . formal) body)`

Create a new function named `var` with one or more named formal parameters, an extra single
formal parameter, and the body `body`.  All extra arguments are converted into a list and
passed as the last parameter.  names may not be reused in a single definition.

`(define-condition-type ty superty constr pred)`

Create a new condition type of the given `ty`.  This does not create a new object instance of
that type.  The new condition must inherit from some other already existing condition type as
given by `superty`.  For many new conditions, `base-error` will suffice as their super type.
`constr` is a name that should be given to a function that will construct instances of this
type, and `pred` is a name that should be given to a function that will determine whether an
object is an instance of this type.

`(div x y)`

Returns the integer division result of `x/y`.  `y` must not be zero.

`(even? n)`

Returns `#t` if `n` is even, otherwise returns `#f`.

`(exists func list)`

Apply `func` to each element of `list` until it returns `#t`.  If the list is empty or no
matching element is found, return `#f`.  Otherwise return `#t`.  **NOTE:** This function
differs from standard scheme.  It only supports one list.  This may be fixed.  It also does
not support returning the matching value.  This may not be fixed.

`(filter proc list)`

Returns all the elements of `list` for which `proc` returns `#t`.  **NOTE:** This function
differs from standard scheme.  It only supports one list.  This may be fixed.

`(find proc list)`

Returns the first element of `list` for which `proc` returns `#t`.  If no element satisfies
this condition, it returns `#f`.

`(fold-left proc init list)`

This function applies `proc` to an accumulator value and each element of `list` from left
to right.  The accumulator value starts with `init` and builds up with each successive
application.  When all elements have been processed, the accumulator value is returned.
`proc` must take two arguments.  **NOTE:** This function differs from standard scheme.  It
only supports one list.  This may be fixed.

`(fold-right proc init list)`

This function applies `proc` to an accumulator value and each element of `list` from right
to left.  The accumulator value starts with `init` and builds up with each successive
application.  When all elements have been processed, the accumulator value is returned.
`proc` must take two arguments.  **NOTE:** This function differs from standard scheme.  It
only supports one list.  This may be fixed.

`(for-all func list)`

Apply `func` to each element of `list` until it returns `#f`.  If the list is empty or all
elements match, return `#t`.  Otherwise return `#f`.  **NOTE:** This function differs from
standard scheme.  It only supports one list.  This may be fixed.  It also does not support
returning the matching value.  This may not be fixed.

`(guard (var (test1 expr1) ... (testN exprN) [(else expr)]) body)`

The guard expression is used for handling exceptional conditions.  The `body` is evaluated.  If
no exception is raised, guard does nothing else and returns the result of the evaluation.  If
an exception is raised, the condition object is bound to `var`.  Each `test` expression is then
evaluated in this environment until one returns `#t`.  When that happens, return the matching
`expr`.  If no `test` passes, return the else `expr` if it exists.  If there is no else
expression, the exception is re-raised.

`(if pred true-expr false-expr)`

Evaluate the `pred` expression.  If it evaluates to `#t`, return the evaluation of the
`true-expr`.  Otherwise, return the evaluation of the `false-expr`.

`(inc n)`

Returns `n` incremented by 1.  **NOTE:** This is a function unique to this implementation of
scheme.  It does not occur in the standard library.

`(integer? obj)`

Returns `#t` only if `obj` is an integer.

`(length list)`

Returns the number of elements in `list`.

`(let ((var1 init1) ... (varN initN)) body)`

Evaluate each `init` expression and bind it to each `var`.  Each variable name may only
be used once in a given let expression.  The `inits` are not evaluated in any guaranteed
order, and bindings may not refer to other bindings in the same let expression.  Returns
the evaluation of `body`.

`(let name ((var1 init1) ... (varN initN)) body)`

Named let is like let, except that `name` is bound in `body` to a function.  This function
takes as parameters all the `vars`, which are initialized with the `inits`, and whose body
is `body`.  This allows for defining a recursive function.

`(list obj1 ... objN)`

Returns a new list containing all its arguments.

`(list? obj)`

Returns `#t` if `obj` is a list, otherwise returns `#f`.

`(list-ref list k)`

Returns the `kth` element of `list`, which must be at least `k+1` in length.

`(list-tail list k)`

Returns `list` after dropping the first `k` elements.  `list` must be at least `k` in length.

`(map proc list)`

Applies `proc` to each element of `list` and returns a new list of the results.  **NOTE:** This
function differs from standard scheme.  It only takes one list.  This may be fixed.

`(max n1 n2 ... nN)`

Returns the largest number from all its arguments.

`(min n1 n2 ... nN)`

Returns the smallest number from all its arguments.

`(mod x y)`

Returns the remainder of the integer division result of `x/y`.  `y` must not be zero.

`(negative? obj)`

Returns `#t` when `obj` is less than zero, otherwise returns `#f`.

`(not obj)`

Returns `#t` if `obj` is `#f`, otherwise returns `#f`.

`(null? obj)

Returns `#t` if `obj` is the empty list, otherwise returns `#f`.

`(number? obj)`

Returns `#t` if `obj` is any numeric type, otherwise returns `#f`.

`(odd? n)`

Returns `#t` if `n` is odd, otherwise returns `#f`.

`(or test1 ... testN)`

Returns `#t` if any test expression evaluates to `#t`.  Otherwise, returns `#f` (including if
there are no test expressions).  **NOTE:** This function differs from standard scheme.  It
does not return the value of the last test.

`(positive? obj)`

Returns `#t` when `obj` is greater than zero, otherwise returns `#f`.

`(procedure? obj)`

Returns `#t` if `obj` is a procedure - a built-in procedure, something defined in the standard
library, a user-defined procedure, or a lambda - and otherwise returns `#f`.

`(quote obj)`

Returns `obj` without evaluating it.

`(raise obj)`

Raise the exception object given by `obj`.  This object must have been previously created with
one of the `make-\*-error` functions.  When an exception is raised, it will propagate all the
way up to the top level where it will cause the interpreter to stop, unless it is handled with
a `guard` expression.

`(real? obj)`

Returns `#t` if `obj` is a floating point (real) number or integer.

`(reverse list)`

Returns a new list consisting of the elements of `list` in reverse order.

`(string? obj)`

Returns `#t` if `obj` is a string, otherwise returns `#f`.

`(string=? str1 str2)`
`(string<? str1 str2)`
`(string>? str1 str2)`
`(string<=? str1 str2)`
`(string>=? str1 str2)`

These functions are used to compare two strings, returning `#t` or `#f` as appropriate.
**NOTE:** These functions differ from standard scheme.  They only take two strings
instead of a variable number of strings.  This may be fixed.

`(string->list str)`

Convert `str` into a list of characters.

`(string-length str)`

Returns the number of characters in `str`.

`(unless test expr)`

Evaluate the `test` expression.  If it evaluates to `#f`, evaluate `expr` and return that as
the result of the entire expression.  Otherwise, return the implementation-defined value of
`#f`.  **NOTE:** This function differs from standard scheme.  Only one expression is
supported.  This may be fixed.

`(when text expr)`

Evaluate the `test` expression.  If it evaluates to `#t`, evaluate `expr` and return that as
the result of the entire expression.  Otherwise, return the implementation-defined value of
`#f`.  **NOTE:** This function differs from standard scheme.  Only one expression is
supported.  This may be fixed.

`(zero? obj)`

Returns `#t` when `obj` is equal to zero, otherwise returns `#f`.
