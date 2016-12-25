; Basic math.
(define (dec x) (- x 1))
(define (inc x) (+ x 1))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (zero? x) (== x 0))

; Booleans.
(define (not x) (if (eqv? x #f) #t #f))

; Lists.
(define (filter fn lst)
        (fold-right (lambda (x xs) (if (fn x) (cons x xs) xs)) '() lst))
(define (fold-left fn accum lst)
        (if (null? lst)
            accum
            (fold-left fn (fn accum (car lst)) (cdr lst))))
(define (fold-right fn accum lst)
        (if (null? lst)
            accum
            (fn (car lst) (fold-right fn accum (cdr lst)))))
(define (length lst) (fold-left inc 0 lst))
(define (map fn lst)
        (if (null? lst)
            '()
            (cons (fn (car lst))
                  (map fn (cdr lst)))))
(define (null? x) (eqv? '() x))

; Functions.
(define (compose f g) (lambda (arg) (f (g arg))))
(define (curry fn arg1) (lambda (arg2) (fn arg1 arg2)))
(define (flip fn) (lambda (arg1 arg2) (fn arg2 arg1)))
(define (id x) x)
