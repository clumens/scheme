`(boolean? obj)`

Returns `#t` if `obj` is a boolean, otherwise returns `#f`.

`(car list)`

Returns the contents of the first element of `list`, which must not be empty.

`(cdr list)`

Returns the contents of the second element of `list`, which must be not empty.

`(char? obj)`

Returns `#t` if `obj` is a character, otherwise returns `#f`.

`(char=? ch1 ch2)`
`(char<? ch1 ch2)`
`(char>? ch1 ch2)`
`(char<=? ch1 ch2)`
`(char>=? ch1 ch2)`

These functions are used to compare two characters, returning `#t` or `#f` as appropriate.
**NOTE:** These functions differ from standard scheme.  They only take two characters
instead of a variable number of characters.  This may be fixed.

`(cons obj1 obj2)`

Returns new list where the first element is `obj1` and the second element is `obj2`, which
must be a list.

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

`(length list)`

Returns the number of elements in `list`.

`(list? obj)`

Returns `#t` if `obj` is a list, otherwise returns `#f`.

`(negative? obj)`

Returns `#t` when `obj` is less than zero, otherwise returns `#f`.

`(not obj)`

Returns `#t` if `obj` is `#f`, otherwise returns `#f`.

`(null? obj)

Returns `#t` if `obj` is the empty list, otherwise returns `#f`.

`(number? obj)`

Returns `#t` if `obj` is a number, otherwise returns `#f`.

`(positive? obj)`

Returns `#t` when `obj` is greater than zero, otherwise returns `#f`.

`(procedure? obj)`

Returns `#t` if `obj` is a procedure - a built-in procedure, something defined in the standard
library, a user-defined procedure, or a lambda - and otherwise returns `#f`.

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
