;; put/getの実装
(define system-table '())
(define (eql? a b)
  (define (list-eq? a b)
    (cond
      ((and (null? a) (null? b)) #t)
      ((or  (null? a) (null? b)) #f)
      ((eql? (car a) (car b)) (list-eq? (cdr a) (cdr b)))
      (else #f)))
  (cond
    ((and (pair? a) (pair? b)) (list-eq? a b))
    ((and (not (pair? a)) (not (pair? b))) (eq? a b))
    (else #f))
  )
(define (get-kv table k)
  (cond
    ((null? table) '())
    ((eql? k (car (car table))) (cdr (car table)))
    (else (get-kv (cdr table) k))))
(define (put-kv table k v)
  (cond
    ((null? table) (list (cons k v)))
    ((eql? k (car (car table))) (cons (cons k v) (cdr table)))
    (else (cons (car table) (put-kv (cdr table) k v)))))
(define (get op type)
  (let ((result (get-kv (get-kv system-table op) type)))
    (if (null? result) #f result)))
(define (put op type item)
  (set! system-table
    (put-kv system-table op (put-kv (get-kv system-table op) type item))))

;; ex2-78
; 目的
;   (add 1 2)ができるようにする
;
; (add 1 2)
; ↓
; (apply-generic 'add x y)
; ↓
; (apply-generic 'add . '(x y))
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;         (apply proc (map contents args))
;
; 必要なこと
; -type-tag x が 'scheme-numberを返す.
;  (get 'add '(scheme-number scheme-number)) でprocを返すため
; -(map contents (x y) が (1 2)を返す.
; -(put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
;  で (+ x y)した値にはタグを付けない

; タグ付きデータ
;;  Schemeの内部型システムが有効な場合はタグを付けない
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    ((cons type-tag contents))))

;;  Schemeの内部型システムの場合には'scheme-numberを返す
(define (type-tag datum)
  (cond
    ((number? datum) 'scheme-number)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum : TYPE-TAG " datum ))))

;;  Schemeの内部型システムの場合にはタグはないものと考える.
(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged datum : CONTENTS " datum ))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types : APPLY-GENERIC "
               (list op type-tags))))))

; 1.汎用性のある四則演算を定義する
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; 2.通常の数字パッケージを定義する.
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number                (lambda (x)   (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; 3.数字パッケージをインストール
(install-scheme-number-package)

; 4.数字パッケージを使用する
(add (make-scheme-number 6) (make-scheme-number 3))
(sub (make-scheme-number 6) (make-scheme-number 3))
(mul (make-scheme-number 6) (make-scheme-number 3))
(div (make-scheme-number 6) (make-scheme-number 3))

; 5.数字パッケージをSchemeの型システムを使用して利用する
(add 6 3)
(sub 6 3)
(mul 6 3)
(div 6 3)


