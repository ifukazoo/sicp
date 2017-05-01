;(variable? e) (symbol? e)) ; eは変数か?
;(same-variable? v1 v2)     ; v1とv2は同じ変数か?
;(make-sum a1 a2)           ; a1とa2の和を構築する
;(make-product m1 m2)       ; m1とm2の積を構築する
;(sum? e)                   ; 和は最初の要素が+であるリストである
;(addend s)                 ; 加数は和のリストの2番目の項である
;(augent s)                 ; 被加数は和のリストの3番目の項である
;(product? e)               ; 積は最初の要素が*であるリストである
;(multiplier p)             ; 乗数は積のリストの2番目の項である
;(multiplicand p)           ; 被乗数は乗数のリストの3番目の項である

; 実装ができているとすると,微分の規則を次のように定義できる
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum
                      (deriv (addend exp) var)
                      (deriv (augent exp) var)))
        ((product? exp) (make-sum
                          (make-product
                            (multiplicand exp)
                            (deriv (multiplier exp) var))
                          (make-product
                            (multiplier exp)
                            (deriv (multiplicand exp) var))))
        (else
          (error "unknown expression type : DERIV" exp))))

; 実装
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (sum? e)
  (and (pair? e)
       (eq? '+ (car e))))
(define (addend s)
  (cadr s))
(define (augent s)
  (caddr s))
(define (product? e)
  (and (pair? e)
       (eq? '* (car e))))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (caddr p))


; テスト
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; 簡約されてないが,正しいものが出てくる.

; make-sumを変更して簡約規則を実装する.
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; テスト
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
