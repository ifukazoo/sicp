(define (make-accmulator init)
  (lambda (n)
    (begin
      (set! init (+ init n))
      init
      )))


; テスト
(define A (make-accmulator 5))
(A 10)
(A 15)
