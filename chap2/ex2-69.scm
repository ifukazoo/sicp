;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 復習
; リストの最後尾から処理する手順
(define (reverse-r lst)
  (if (null? lst)
    lst
    (append (reverse-r (cdr lst)) (list (car lst)))))
(reverse-r '(1 2 3 4 5))

(define (reverse-i lst)
  (define (reverse-i-1 lst acc)
    (if (null? lst)
      acc
      (reverse-i-1 (cdr lst) (cons (car lst) acc))))
  (reverse-i-1 lst '()))
(reverse-i '(1 2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 解答
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
(define tree (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))

; 左枝を1,右枝を0とすると,この木の符号化はこうなる
; A 0
; B 1 1 1
; C 1 1 0 0
; D 1 1 0 1
; E 1 0 1 0
; F 1 0 1 1
; G 1 0 0 0
; H 1 0 0 1

; 対応する encode,decodeを考える.
; 左と右が教科書の例と逆にするだけで,符号化復号化に影響ないのがポイント
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
(define (include? sym lst)
  (cond ((null? lst) #f)
        ((eq? sym (car lst)) #t)
        (else (include? sym (cdr lst)))))
(define (encode-symbol sym tree)
  (if (leaf? tree)
    '()
    (let ((left (left-branch tree)) (right (right-branch tree)))
      (cond
        ((include? sym (symbols left))  (cons 1 (encode-symbol sym left)))
        ((include? sym (symbols right)) (cons 0 (encode-symbol sym right)))
        (else (error "bad tree" tree))))))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond
      ((= bit 1) (left-branch branch))
      ((= bit 0) (right-branch branch))
      (else (error "bad bit:CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(encode '(B A C A D A E A F A B B A A A G A H) tree)
(decode (encode '(B A C A D A E A F A B B A A A G A H) tree) tree)
