; 2.4.2 タグ付きデータ
; 2つの表現方法でもって,利用者側にどちらの表現を使っているのかを意識させない
; 方法はわかった.
; 今度は,2つの表現方法を同じシステムに共存させる方法を学ぶ.

; タグ付きデータを利用する
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum : TYPE-TAG " datum )))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum : CONTENTS " datum )))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))


; セレクタ

; 直交式 の修正版
(define (make-from-real-imag-rectanglar x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectanglar r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) (square (real-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (/ (imag-part-rectangular z) (imag-part-rectangular z))))

; 極形式の修正版
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan (/ y x)))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))
(define (real-part-polar z) (* (car z) (cos (cdr z))))
(define (imag-part-polar  z) (* (car z) (sin (cdr z))))
(define (magnitude-polar z) (car z))
(define (angle-polar     z) (cdr z))

; ジェネリックセレクタ
(define (real-part z)
  (cond
    ((rectangular? z) (real-part-rectangular (contents z)))
    ((polar? z) (real-part-polar (contents z)))
    (else (error "unknown type: real-part" z))))
(define (imag-part  z)
  (cond
    ((rectangular? z) (imag-part-rectangular (contents z)))
    ((polar? z) (imag-part-polar (contents z)))
    (else (error "unknown type: imag-part" z))))

(define (magnitude z)
  (cond
    ((rectangular? z) (magnitude-rectangular (contents z)))
    ((polar? z) (magnitude-polar (contents z)))
    (else (error "unknown type: magnitude" z))))

(define (angle     z)
  (cond
    ((rectangular? z) (angle-rectangular (contents z)))
    ((polar? z) (angle-polar (contents z)))
    (else (error "unknown type: angle" z))))

; ジェネリックコンストラクタ
(define (make-from-real-imag x y)
  (make-from-real-imag-rectanglar x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; 四則演算は変更しない.
; 呼び出すセレクタはジェネリックだから
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

(define (degree-measure rad)
  (* (/ rad 3.14) 180))
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

; ----- add-complex, sub-complex, mul-complex, div-complex -----
;                            ^
;                            |
;                            v
; ----- real-part, imag-part, magnitude, angle             -----
;                            ^
;                            | タグ
;                            v
; ----- 直交形式による実装   |  極形式による実装           -----

; 上のレベルとの境界でタグがついたり外れたりしていることに注目
; =>型によるディスパッチ.
;   ただ,2.4.2の方法には弱点が2つある
;   1)インターフェース手続きが,すべての表現を知っておく必要がある.
;   2)個々の実装は個別に設計できるが,名前がシステム全体で被らないよう注意が必要
; =>加法的ではない.
