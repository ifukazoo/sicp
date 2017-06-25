(define (equal_? a b)
  (cond ((not (or (pair? a) (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal_? (car a) (car b)) (equal_? (cdr a) (cdr b))))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;テスト
(equal_? 'a 'b)
(equal_? '(a) '(b))
(equal_? '() '())
(equal_? '(a b) '(a b))
(equal_? 'a '(a b))
