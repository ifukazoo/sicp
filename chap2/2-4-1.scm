; 2.4 抽象データの多重表現

; 複素数を直交式,極形式,の2種類の表現で実装し,
; システムに加法的に追加,利用できる方法を学ぶ.
; =>Open-Closedの原則?

; 2.4.1 複素数の表現
; 複素数の四則演算を考える.
; 実部   のセレクタ real-part
; 虚部   のセレクタ imag-part
; 絶対値 のセレクタ magnitude
; 偏角   のセレクタ angle
; とし,
; たし算,ひき算 => 直交式
; かけ算,わり算 => 極形式
; で演算を行う

(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

; 直交式 で実装したBen
; セレクタ
(define (real-part z) (car z))
(define (imag-part  z) (cdr z))
; r = √x^2 + y^2
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
; θ = arctan(y/x)
(define (angle z)
  (atan (/ (imag-part z) (real-part z))))

(define (make-from-real-imag x y)
  (cons x y))
; x = r * cos(a)
; y = r * sin(a)
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define z1 (make-from-real-imag 4 3))
(define z2 (make-from-mag-ang 5 (atan (/ 3 4))))
(define (degree-measure rad)
  (* (/ rad 3.14) 180))
(real-part z1)
(imag-part z1)
(magnitude z1)
(degree-measure (angle z1))

(real-part z2)
(imag-part z2)
(magnitude z2)
(degree-measure (angle z2))

(define z3 (add-complex z1 z2))
(real-part z3)
(imag-part z3)
(magnitude z3)
(degree-measure (angle z3))

; 極形式 で実装したAlyssa
; セレクタ
; x = √x^2 + y^2
; θ = arctan(y/x)
(define (magnitude z)
  (car z))
(define (angle z)
  (cdr z))
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part  z)
  (* (magnitude z) (sin (angle z))))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan (/ y x))))
(define (make-from-mag-ang r a)
  (cons r a))

(define z1 (make-from-real-imag 4 3))
(real-part z1)
(imag-part z1)
(magnitude z1)
(degree-measure (angle z1))

(define z2 (make-from-mag-ang 5 (atan (/ 3 4))))
(real-part z2)
(imag-part z2)
(magnitude z2)
(degree-measure (angle z2))

(define z3 (add-complex z1 z2))
(real-part z3)
(imag-part z3)
(magnitude z3)
(degree-measure (angle z3))
