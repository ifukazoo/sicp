; 2.3.4 例:ハフマン符号化木

;; 頭がこんがらがったので整理
;; 1)文字に対して頻度の重みをつける.
;; 2)ハフマン符号化木が作成できる(いわば表).
;; 3)2)の木に対する符号化,復号化の実装を検討できる.

;; この章には符号化の関数の説明がないので混乱したのが原因

; 葉の表現
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

; 木(分岐)の表現
;   配下のシンボルを和集合にして持ち,配下の重みの合算値を持つ
(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; 符号化手続き
;   左枝が含んでいた場合0を足す
;     葉の場合木の先頭に戻る
;   右枝が含んでいた場合1を足す
;     葉の場合木の先頭に戻る
;
; ※ 後日記載 練習問題2.68に,よりシンプルな方法あり
(define (include? sym lst)
  (cond
    ((null? lst) #f)
    ((eq? sym (car lst)) #t)
    (else (include? sym (cdr lst)))))
(define (encode lst tree)
  (define (encode-1 lst current-branch)
    (if (null? lst)
      '()
      (let ((sym (car lst)) (left (left-branch current-branch)) (right (right-branch current-branch)))
        (cond
          ((include? sym (symbols left))
           (if (leaf? left)
             (cons 0 (encode-1 (cdr lst) tree))
             (cons 0 (encode-1 lst left))))
          ((include? sym (symbols right))
           (if (leaf? right)
             (cons 1 (encode-1 (cdr lst) tree))
             (cons 1 (encode-1 lst right))))
          (else (error "bad lst:encode" lst))))))
  (encode-1 lst tree))

; 複合化手続き
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond
      ((= bit 0) (left-branch branch))
      ((= bit 1) (right-branch branch))
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

(define huffman
  (make-code-tree
    (make-leaf 'a 8)
    (make-code-tree
      (make-code-tree
        (make-leaf 'b 3)
        (make-code-tree
          (make-leaf 'c 1)
          (make-leaf 'd 1)))
      (make-code-tree
        (make-code-tree
          (make-leaf 'e 1)
          (make-leaf 'f 1))
        (make-code-tree
          (make-leaf 'g 1)
          (make-leaf 'h 1)))
      )))
(define bits (list 1 0 0 0 1 0 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 0 1 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 1))
(define str  '(b a c a d a e a f a b b a a a g a h))
(decode bits huffman)
(encode str huffman)
(decode (encode str huffman) huffman)

; 重みつき要素の集合

(define (weight leaf) (caddr leaf))
(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((< (weight (car set)) (weight x)) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((head (car pairs)))
      (adjoin-set (make-leaf (car head) (cadr head))
                  (make-leaf-set (cdr pairs))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))



(make-leaf 'A 4)
(make-leaf 'B 2)
(make-leaf 'C 1)
(make-leaf 'D 1)

(adjoin-set (make-leaf 'B 2) '())
(adjoin-set (make-leaf 'C 1)
            (adjoin-set (make-leaf 'B 2) '()))
(adjoin-set (make-leaf 'A 4)
            (adjoin-set (make-leaf 'C 1)
                        (adjoin-set (make-leaf 'B 2) '())))
(adjoin-set (make-leaf 'D 1)
            (adjoin-set (make-leaf 'A 4)
                        (adjoin-set (make-leaf 'C 1)
                                    (adjoin-set (make-leaf 'B 2) '()))))
