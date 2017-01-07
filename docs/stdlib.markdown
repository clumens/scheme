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

`(apply proc arg1 ... rest-args)`

Call the procedure `proc` with the given arguments.  `rest-args` is the only argument that
must be present, and it must be a list.

`(begin expr1 ... exprN)`

Evaluate all expressions, returning the value of the last one.  This is mainly used to include
side effects before returning some value.

`(boolean? obj)`

Returns `#t` if `obj` is a boolean, otherwise returns `#f`.

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

`(div x y)`

Returns the integer division result of `x/y`.  `y` must not be zero.

`(even? n)`

Returns `#t` if `n` is even, otherwise returns `#f`.

`(exists func list)`

Apply `func` to each element of `list` until it returns `#t`.  If the list is empty or no
matching element is found, return `#f`.  Otherwise return `#t`.  **NOTE:** This function
differs from standard scheme.  It only supports one list.  This may be fixed.  It also does
not support returning the matching value.  This may not be fixed.

`(for-all func list)`

Apply `func` to each element of `list` until it returns `#f`.  If the list is empty or all
elements match, return `#t`.  Otherwise return `#f`.  **NOTE:** This function differs from
standard scheme.  It only supports one list.  This may be fixed.  It also does not support
returning the matching value.  This may not be fixed.

`(if pred true-expr false-expr)`

Evaluate the `pred` expression.  If it evaluates to `#t`, return the evaluation of the
`true-expr`.  Otherwise, return the evaluation of the `false-expr`.

`(inc n)`

Returns `n` incremented by 1.  **NOTE:** This is a function unique to this implementation of
scheme.  It does not occur in the standard library.

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

`(mod x y)`

Returns the remainder of the integer division result of `x/y`.  `y` must not be zero.

`(negative? obj)`

Returns `#t` when `obj` is less than zero, otherwise returns `#f`.

`(not obj)`

Returns `#t` if `obj` is `#f`, otherwise returns `#f`.

`(null? obj)

Returns `#t` if `obj` is the empty list, otherwise returns `#f`.

`(number? obj)`

Returns `#t` if `obj` is a number, otherwise returns `#f`.

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
