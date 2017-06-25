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
(define tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))
tree
; ビット表
; 0     NA
; 10    YIP
; 1101  A
; 1110  SHA
; 11000 WAH
; 11001 BOOM
; 11110 GET
; 11111 JOB
(define song '(
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
               SHA BOOM
               ))
(length song)
(define encodedsong (encode song tree))
encodedsong
(length encodedsong)
(decode encodedsong tree)

; 符号化には84ビット必要

; 固定長符号を使用した場合
; 3ビット x 36 = 108ビット
