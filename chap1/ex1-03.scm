(define (top2-sum a b c)
  (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
        ((and (< b c) (< b a)) (+ (square c) (square a)))
        (else                  (+ (square a) (square b)))))

; test
(display (top2-sum 0 1 2))
(newline)

(display (top2-sum 1 2 0))
(newline)

(display (top2-sum 2 0 1))
(newline)

(display (top2-sum 2 2 1))
(newline)
