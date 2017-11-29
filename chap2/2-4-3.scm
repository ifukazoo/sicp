; 2.4.3 データ手動プログラミングと加法性

;                             型
;           | 極形式           | 直交形式              |
; real-part | real-part-polar  | real-part-rectangular |
; imag-part | imag-part-polar  | imag-part-rectangular |
; magnitude | magnitude-polar  | magnitude-rectangular |
; angle     | angle-polar      | angle-rectangular     |

; 2017/07/02 06:21
; わからない点
; ・引数の型をリストにするか,しないのか
;   -> ※1,※2を見る.まだ完璧には理解できてないが.

; ・コンストラクタだけインストールにlambda式にするのは?
;   -> 型をタグ付けする手続きを追加するため

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 前提の定義
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 個別の実装
;; 直交式
(define (install-rectangler-package)
  ; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part  z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (/ (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  ;;;; 手続きの名前         手続き引数     実装
  (put 'real-part           '(rectangular) real-part)
  (put 'imag-part           '(rectangular) imag-part)
  (put 'magnitude           '(rectangular) magnitude)
  (put 'angle               '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;; 極形式
(define (install-polar-package)
  ; 内部手続き
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part  z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan (/ y x))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))

  ; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  ;;;; 手続きの名前         手続き引数  実装
  (put 'real-part           '(polar)    real-part)
  (put 'imag-part           '(polar)    imag-part)
  (put 'magnitude           '(polar)    magnitude)
  (put 'angle               '(polar)    angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージシステムを使用するためのシステム定義
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types : APPLY-GENERIC "
               (list op type-tags))))))

;; セレクタ
;;   zがどの型を持っているか,気にする必要はないし,
;;   新しい型が追加になっても問題はない
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle     z))
;; ※1
;;   例えば,セレクタではない複数の引数を持つ手続きをつくった場合,
;;   それぞれが型を持つ形となり,パッケージシステムインストール時に
;;   リストにしたことが効いてくる.

;; コンストラクタ
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang   'polar) r a))
;; ※2
;;   コンストラクタでは,最終的にどの実装を使うかを決定する必要があるので
;;   パッケージシステム実装がむき出しになる(get が登場)ということか?.
;;   型は1つしか指定しないので,インストール時も型をリストにする必要がない.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テスト
(install-rectangler-package)
(install-polar-package)
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





