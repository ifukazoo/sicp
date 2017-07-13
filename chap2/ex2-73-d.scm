;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        (else ((get (operator exp) 'deriv) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 変更点は,putのopとtypeを逆にするだけである.
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
  (put '+ 'deriv  deriv-sum)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト
(install-sum-package)

(deriv '(+ x 3) 'x)
