(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum
                      (deriv (addend exp) var)
                      (deriv (augent exp) var)))
        ((product? exp) (make-sum
                          (make-product
                            (multiplier exp)
                            (deriv (multiplicand exp) var))
                          (make-product
                            (deriv (multiplier exp) var)
                            (multiplicand exp))))
        ((exponentiation? exp) (make-product
                                 (exponent exp)
                                 (make-product
                                   (make-exponentiation
                                     (base exp)
                                     (make-sum (exponent exp) -1))
                                   (deriv (base exp) var))))
        (else
          (error "unknown expression type : DERIV" exp))))

; 実装
;; 判別式
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? e)
  (and (pair? e) (eq? '+ (cadr e))))
(define (product? e)
  (and (pair? e) (eq? '* (cadr e))))
(define (exponentiation? e)
  (and (pair? e) (eq? '** (cadr e))))

;; セレクタ
(define (addend s) (car s))
(define (augent s) (caddr s))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (base e) (car e))
(define (exponent e) (caddr e))

;; コンストラクタ
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list b '** e))))
;===============================================================================
; テスト
(make-sum 1 2)
(make-sum 'a 'b)
(sum? (make-sum 'a 'b))

; x + (3 * (x + y + 2))
(deriv '(x + (3 * (x + (y + 2)))) 'x)

; x^3 + 2x^2 + 3x + 1
(deriv '((x ** 3) + ((2 * (x ** 2)) + ((3 * x) + 1))) 'x)

