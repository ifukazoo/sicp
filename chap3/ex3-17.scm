(define (count-pairs x)
  (define (find x lst)
    (cond
      ((null? lst) #f)
      ((eq? x (car lst)) #t)
      (else (find x (cdr lst)))))
  (define counted '())
  (define (count-pairs-taking-records x)
    (if (not (pair? x))
      0
      (if (find x counted)
        (begin
          (+
            (count-pairs-taking-records (car x))
            (count-pairs-taking-records (cdr x))
            0)
          )
        (begin
          (set! counted (cons x counted))
          (+
            (count-pairs-taking-records (car x))
            (count-pairs-taking-records (cdr x))
            1)
          )
        )
      )
    )
  (count-pairs-taking-records x)
  )

; 3
(define a (cons 'a '()))
(define b (cons 'b '()))
(define x (cons a b))
(count-pairs x)

; (find x)

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


