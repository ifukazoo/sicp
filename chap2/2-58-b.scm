(define (deriv exp var)
  (cond
    ((number? exp) 0)
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

;;; sum? と product?は, 記号を探す実装に変更
(define (find sym lst)
  (cond ((null? lst) #f)
        ((eq? sym (car lst)) #t)
        (else (find sym (cdr lst)))))
(define (product? e)
  (and (pair? e) (find '* e)))
(define (sum? e)
  (and (pair? e) (find '+ e)))

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

;; セレクタ
;;; 記号の左辺,右辺をリストにして返すが,要素が1つの場合はリストから出して返す
(define (leftside sym)
  (lambda (exp)
    (define (leftside_ e lst)
      (if (eq? sym (car e))
        (if (null? (cdr lst))
          (car lst)
          (reverse lst))
        (leftside_ (cdr e) (cons (car e) lst))))
    (leftside_ exp '())))
(define (rightside sym)
  (define (rightside_ exp)
    (if (eq? sym (car exp))
      (if (null? (cddr exp))
        (cadr exp)
        (cdr exp))
      (rightside_ (cdr exp))))
  rightside_)
(define (multiplier   p) ((leftside  '*) p))
(define (addend       s) ((leftside  '+) s))
(define (multiplicand p) ((rightside '*) p))
(define (augent       s) ((rightside '+) s))

;===============================================================================
; テスト
(deriv '(x + 3 * (x + y + 2)) 'x)
