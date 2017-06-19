;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 前提の定義
(define (symbol-leaf leaf) (cadr leaf))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (weight-leaf leaf) (caddr leaf))
(define (leaf? object) (eq? (car object) 'leaf))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))
(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((> (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((head (car pairs)))
      (adjoin-set (make-leaf (car head) (cadr head))
                  (make-leaf-set (cdr pairs))))))
(define (successive-merge leaf-set)
  (define (successive-merge-i set ordered)
    (cond
      ((null? set) '())
      ((null? (cdr set))  (car set))
      ((null? (cddr set)) (successive-merge-i (adjoin-set (make-code-tree (car set) (cadr set)) ordered) '()))
      (else (successive-merge-i (cdr set) (adjoin-set (car set) ordered)))))
  (successive-merge-i leaf-set '()))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 解答
(define (pow2 n) (if (= n 0) 1 (* 2 (pow2 (- n 1)))))
(define (make-pair n) (cons n (list (pow2 (- n 1)))))
(define (make-pairs n) (if (= 0 n) '() (cons (make-pair n) (make-pairs (- n 1)))))

(generate-huffman-tree (make-pairs 5))
; 5 1
; 4 0 1
; 3 0 0 1
; 2 0 0 0 1
; 1 0 0 0 0
(generate-huffman-tree (make-pairs 10))
; 10 1
;  9 0 1
;  8 0 0 1
;  ...
;  2 0 0 0 0 0 0 0 0 1
;  1 0 0 0 0 0 0 0 0 0

; 最も頻度の高い記号 1bit
; 最も頻度の低い記号 n-1bit

