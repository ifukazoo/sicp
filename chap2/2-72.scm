; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; 2.68
; (define (left-branch tree) (car tree))
; (define (right-branch tree) (cadr tree))
; (define (weight-leaf leaf) (caddr leaf))
; (define (symbol-leaf leaf) (cadr leaf))
; (define (leaf? object) (eq? (car object) 'leaf))
; (define (symbols tree)
;   (if (leaf? tree)
;     (list (symbol-leaf tree))
;     (caddr tree)))
; (define (weight tree)
;   (if (leaf? tree)
;     (weight-leaf tree)
;     (cadddr tree)))
; (define (make-leaf symbol weight) (list 'leaf symbol weight))
; (define (make-code-tree left right)
;   (list left right
;         (append (symbols left) (symbols right))
;         (+ (weight left) (weight right))))

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
        ((include? sym (symbols right)) (cons 1 (encode-symbol sym right)))
        ((include? sym (symbols left))  (cons 0 (encode-symbol sym left)))
        (else (error "bad tree" tree))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(trace include?)
(encode '(C) sample-tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト
; (encode '(A B C D) sample-tree)
;
; (define (decode bits tree)
;   (define (choose-branch bit branch)
;     (cond
;       ((= bit 0) (left-branch branch))
;       ((= bit 1) (right-branch branch))
;       (else (error "bad bit:CHOOSE-BRANCH" bit))))
;   (define (decode-1 bits current-branch)
;     (if (null? bits)
;       '()
;       (let ((next-branch
;               (choose-branch (car bits) current-branch)))
;         (if (leaf? next-branch)
;           (cons (symbol-leaf next-branch)
;                 (decode-1 (cdr bits) tree))
;           (decode-1 (cdr bits) next-branch)))))
;   (decode-1 bits tree))
;
; (decode (encode '(A B C D) sample-tree) sample-tree)
; (decode (encode '(D C B A) sample-tree) sample-tree)
; (decode (encode '(A A A A) sample-tree) sample-tree)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
頻度が最大の記号
encode        1
encode-symbol 1
leaf?         1
left-branch   1
right-branch  1
include?      n ;O(1)のようにも思えるが,頻度が高い記号が左右どちらに配置するかは
;                自明出ないので,記号が入っていない枝を空打ちすることが考えられる
O(n)

頻度が最小の記号
encode        1
encode-symbol n
leaf?         n
left-branch   n
right-branch  n
include?      n*n
O(n^2)
