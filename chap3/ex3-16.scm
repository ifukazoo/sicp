(define (count-pairs x)
  (if (not (pair? x))
    0
    (+
      (count-pairs (car x))
      (count-pairs (cdr x))
      1)))

; 3
(define a (cons 'a '()))
(define b (cons 'b '()))
(define x (cons a b))
(count-pairs x)

; 4
(define a (cons 'a '()))
(define b (cons 'b '()))
(define x (cons a b))
(set-cdr! a b)
(count-pairs x)

;7
(define b (cons 'b '()))
(define a (cons b b))
(define x (cons a a))
(count-pairs x)

