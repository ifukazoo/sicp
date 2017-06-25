(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  ; 重複を許すのでチェックの必要がなくなる
  ; (if (element-of-set? x set)
  ;   set
  ;   (cons x set)))
  (cons x set))

(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((element-of-set? (car set1) set2)
     (cons (car set1) (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
  ; 単に連結するだけでよくなる
  ; (cond
  ;   ((null? set1) set2)
  ;   ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
  ;   (else (cons (car set1) (union-set (cdr set1) set2)))
  ;   ))
  (append set1 set2))

;===============================================================================
; テスト
(define a '(2 3 2 1 3 2 2))
(define b '(3 4 3 2 4 3 3))
(intersection-set a b)
(union-set a b)
