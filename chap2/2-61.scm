(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (car set)) #t)
    ((< x (car set)) #f)
    (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x (car set1)) (y (car set2)))
      (cond
        ((= x y) (cons x (intersection-set (cdr set1) (cdr set2))))
        ((< x y)         (intersection-set (cdr set1) set2))
        (else            (intersection-set (cdr set2) set1)) ;(< y x)
        ))))

(define (adjoin-set x set)
  (if (null? set)
    (cons x set)
    (let ((y (car set)))
      (cond
        ((= x y) set)
        ((< x y) (cons x set))
        (else (cons y (adjoin-set x (cdr set))))
        ))))

;===============================================================================
; テスト

'(1 2 3 4 5 6 7 8 9 10)
(element-of-set? 6 '(1 2 3 4 5 6 7 8 9 10))
(element-of-set? 0 '(1 2 3 4 5 6 7 8 9 10))
(element-of-set? 11 '(1 2 3 4 5 6 7 8 9 10))
'(2 5 9 10)
'(1 2 3 5 6 8 9)
(intersection-set '(2 5 9 10) '(1 2 3 5 6 8 9))
(adjoin-set 1 '(2 5 9 10))
(adjoin-set 3 '(2 5 9 10))
(adjoin-set 6 '(2 5 9 10))
(adjoin-set 11 '(2 5 9 10))
