;コンストラクタ
(define (make-tree entry left right)
  (list entry left right))

; セレクタ
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))

;
(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry set)) #t)
    ((< x (entry set)) (element-of-set? x (left-branch set)))
    (else              (element-of-set? x (right-branch set))) ;(> x (entry set))
    ))

(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set))
     (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
    (else
      (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))));(< x (entry set))
    ))

(define (tree-list-1 set)
  (if (null? set)
    '()
    (append (tree->list-1 (left-branch set))
            (cons (entry set) (tree->list-1 (right-branch set))))))
(define (tree-list-2 set)
  (define (helper set result)
    (if (null? set)
      '()
      (helper (left-branch set)
              (cons (entry set) (helper (right-branch set) result)))))
  (helper set '()))

;===============================================================================
; テスト
(define a (adjoin-set 11 (adjoin-set 5 (adjoin-set 1 (adjoin-set 9 (adjoin-set 3 (adjoin-set 7 '())))))))
(define b (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))
(define c (adjoin-set 11 (adjoin-set 7 (adjoin-set 1 (adjoin-set 9 (adjoin-set 3 (adjoin-set 5 '())))))))
; (define d
; (adjoin-set 85 (adjoin-set 1 (adjoin-set 58 (adjoin-set 28 (adjoin-set 23
; (adjoin-set 43 (adjoin-set 92 (adjoin-set 0 (adjoin-set 36 (adjoin-set 48
; (adjoin-set 10 (adjoin-set 31 (adjoin-set 34 (adjoin-set 13 (adjoin-set 1
; (adjoin-set 21 (adjoin-set 32 (adjoin-set 38 (adjoin-set 77 (adjoin-set 66
; (adjoin-set 45 (adjoin-set 99 (adjoin-set 29 (adjoin-set 44 (adjoin-set 62
; (adjoin-set 94 (adjoin-set 27 (adjoin-set 59 (adjoin-set 67 (adjoin-set 4
; (adjoin-set 37 (adjoin-set 44 (adjoin-set 40 (adjoin-set 99 (adjoin-set 10
; (adjoin-set 61 (adjoin-set 44 (adjoin-set 70 (adjoin-set 44 (adjoin-set 2
; (adjoin-set 99 (adjoin-set 20 (adjoin-set 22 (adjoin-set 16 (adjoin-set 31 '()
; ))))))))))))))))))))))))))))))))))))))))))))))
(tree->list-1 a)
(tree->list-2 a)
(tree->list-1 b)
(tree->list-2 b)
(tree->list-1 c)
(tree->list-2 c)
(tree->list-1 d)
(tree->list-2 d)

; 回答
; a:2つとも同じ
; b:解けなかった.
;   b < a らしい
