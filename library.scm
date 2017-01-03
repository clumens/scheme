; Basic math.
; NEEDS TESTS
(define (abs x) (if (positive? x) x (- 0 x)))
; NEEDS TESTS
(define (dec x) (- x 1))
; NEEDS TESTS
(define (inc x) (+ x 1))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (zero? x) (= x 0))

; Booleans.
(define (not x) (if (eqv? x #f) #t #f))

; Control.
(define (unless test expr) (if test #f expr))
(define (when test expr) (if test expr #f))

; Lists.
(define (exists fn lst)
        (cond ((null? lst)      #f)
              ((fn (car lst))   #t)
              (else             (exists fn (cdr lst)))))
; NEEDS TESTS
(define (filter fn lst)
        (fold-right (lambda (x xs) (if (fn x) (cons x xs) xs)) '() lst))
; NEEDS TESTS
(define (find fn lst)
        (cond ((null? lst)      #f)
              ((fn (car lst))   (car lst))
              (else             (find fn (cdr lst)))))
; NEEDS TESTS
(define (fold-left fn accum lst)
        (if (null? lst)
            accum
            (fold-left fn (fn accum (car lst)) (cdr lst))))
; NEEDS TESTS
(define (fold-right fn accum lst)
        (if (null? lst)
            accum
            (fn (car lst) (fold-right fn accum (cdr lst)))))
(define (for-all fn lst)
        (cond ((null? lst)      #t)
              ((fn (car lst))   (for-all fn (cdr lst)))
              (else             #f)))
(define (length lst) (fold-left inc 0 lst))
; NEEDS TESTS
(define (map fn lst)
        (if (null? lst)
            '()
            (cons (fn (car lst))
                  (map fn (cdr lst)))))
(define (null? x) (eqv? '() x))
(define (reverse lst)
        (fold-left (flip cons) '() lst))

; Extremely tedious car/cdr variants.
; NEEDS TESTS (all of these)
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

; Strings
(define (string=? s1 s2)
        (let loop ((lst1 (string->list s1))
                   (lst2 (string->list s2)))
             (cond ((and (null? lst1) (null? lst2))         #t)
                   ((or  (null? lst1) (null? lst2))         #f)
                   ((not (char=? (car lst1) (car lst2)))    #f)
                   (else                                    (loop (cdr lst1) (cdr lst2))))))
(define (string<? s1 s2)
        (let loop ((lst1 (string->list s1))
                   (lst2 (string->list s2)))
             (cond ((and (null? lst1) (null? lst2))         #f)
                   ((null? lst1)                            #t)
                   ((null? lst2)                            #f)
                   ((char<? (car lst1) (car lst2))          #t)
                   (else                                    (loop (cdr lst1) (cdr lst2))))))
(define (string>? s1 s2)
        (let loop ((lst1 (string->list s1))
                   (lst2 (string->list s2)))
             (cond ((and (null? lst1) (null? lst2))         #f)
                   ((null? lst1)                            #f)
                   ((null? lst2)                            #t)
                   ((char>? (car lst1) (car lst2))          #t)
                   (else                                    (loop (cdr lst1) (cdr lst2))))))
(define (string<=? s1 s2)
        (let loop ((lst1 (string->list s1))
                   (lst2 (string->list s2)))
             (cond ((and (null? lst1) (null? lst2))         #t)
                   ((null? lst1)                            #t)
                   ((null? lst2)                            #f)
                   ((not (char<=? (car lst1) (car lst2)))   #f)
                   (else                                    (loop (cdr lst1) (cdr lst2))))))
(define (string>=? s1 s2)
        (let loop ((lst1 (string->list s1))
                   (lst2 (string->list s2)))
             (cond ((and (null? lst1) (null? lst2))         #t)
                   ((null? lst1)                            #f)
                   ((null? lst2)                            #t)
                   ((not (char>=? (car lst1) (car lst2)))   #f)
                   (else                                    (loop (cdr lst1) (cdr lst2))))))
(define (string-length s) (length (string->list s)))

; Functions.
; NEEDS TESTS
(define (compose f g) (lambda (arg) (f (g arg))))
; NEEDS TESTS
(define (curry fn arg1) (lambda (arg2) (fn arg1 arg2)))
; NEEDS TESTS
(define (flip fn) (lambda (arg1 arg2) (fn arg2 arg1)))
; NEEDS TESTS
(define (id x) x)
