;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put/getの実装
(define (eql? a b)
  (define (list-eq? a b)
    (cond
      ((and (null? a) (null? b)) #t)
      ((or  (null? a) (null? b)) #f)
      ((eql? (car a) (car b)) (list-eq? (cdr a) (cdr b)))
      (else #f)))
  (cond
    ((and (pair? a) (pair? b)) (list-eq? a b))
    ((not (or (pair? a) (pair? b))) (eq? a b))
    (else #f))
  )
(define (head t) (car t))
(define (tail t) (cdr t))
(define (key kv) (car kv))
(define (val kv) (cdr kv))
(define (make-kv k v) (cons k v))

(define (put-kv table k v)
  (cond
    ((null? table) (list (make-kv k v)))
    ((eql? k (key (head table))) (cons (make-kv k v) (tail table))) ; replace
    (else (cons (make-kv k v) table))))

(define (get-kv table k)
  (cond
    ((null? table) '())
    ((eql? k (key (head table))) (val (head table)))
    (else (get-kv (tail table) k))))

;;; システムテーブルインターフェース
(define system-table '())
(define (get op type)
  (let ((result (get-kv (get-kv system-table op) type)))
    (if (null? result) #f result)))
(define (put op type item)
  (set! system-table
    (put-kv system-table op (put-kv (get-kv system-table op) type item))))

; a.
;  number? と variable?はタグのないデータの演算なのでディスパッチできない

; b.
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; たし算パッケージ
(define (install-sum-package)
  (define (make a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augent s) (cadr s))
  (define (deriv-sum exp var)
    (make
      (deriv (addend exp) var)
      (deriv (augent exp) var)))
  (put 'deriv '+  deriv-sum)
  'done)

; かけ算パッケージ
(define (install-product-package)
  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (list '* a1 a2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s))
  (define (deriv-product exp var)
    (make-sum
      (make-product
        (multiplier exp)
        (deriv (multiplicand exp) var))
      (make-product
        (deriv (multiplier exp) var)
        (multiplicand exp))))
  (put 'deriv '*  deriv-product)
  'done)

; c.
; 指数パッケージ
(define (power b e)
  (cond
    ((= b 0) 0)
    ((= b 1) 1)
    ((= e 0) 1)
    ((= e 1) b)
    (else (* b (power b (- e 1))))))
(define (install-exponentiation-package)
  (define (make-exponentiation b e)
    (cond
      ((and (number? b) (number? e)) (power b e))
      ((=number? b 0) 0)
      ((=number? b 1) 1)
      ((=number? e 0) 1)
      ((=number? e 1) b)
      (else (list '^ b e))))
  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (list '* a1 a2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (deriv-exponentiation exp var)
    (make-product
      (exponent exp)
      (make-exponentiation (base exp) (- (exponent exp) 1))))
  (put 'deriv '^  deriv-exponentiation)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト
(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(^ x 3) 'x)
