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

; タグ付きデータ
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
;; ex2-80
(define (=zero? x) (apply-generic '=zero? x))

; 2.通常の数字パッケージを定義する.
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  ;; ex2-80
  (put '=zero? '(scheme-number)            (lambda (x)   (= 0 x)))
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
;; ex2-80
(=zero? (make-scheme-number 6))
(=zero? (make-scheme-number 0))


; 5.有理数パッケージを定義する
(define (install-rational-number-package)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; ex2-80
  (define (=zero?-rat x)
    (= 0 (numer x)))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  ;; ex2-80
  (put '=zero? '(rational)       (lambda (x)   (=zero?-rat x)))
  (put 'make 'rational           (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

; 7.有理数パッケージをインストール
(install-rational-number-package)

; 8.有理数パッケージを使用する
(add (make-rational 2 3) (make-rational 1 2))
(sub (make-rational 2 3) (make-rational 1 2))
(mul (make-rational 2 3) (make-rational 1 2))
(div (make-rational 2 3) (make-rational 1 2))
;; ex2-80
(=zero? (make-rational 2 3))
(=zero? (make-rational 0 3))

; !!!複素数パッケージはちょっとややこしい!!!
; 9. 直交形式パッケージを定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

; 10.直交形式パッケージをインストール
;    ただまだインターフェースを定義していないので
;    外部からは使えない.
(install-rectangler-package)

; 11. 極形式パッケージを定義
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

; 12.極形式パッケージをインストール
;    ただまだインターフェースを定義していないので
;    外部からは使えない.
(install-polar-package)

; 13.2つの複素数パッケージを利用できるようにインターフェースを定義する.
(define (add-complex x y)
  (make-from-real-imag
    (+ (real-part x) (real-part y))
    (+ (imag-part x) (imag-part y))))
(define (sub-complex x y)
  (make-from-real-imag
    (- (real-part x) (real-part y))
    (- (imag-part x) (imag-part y))))
(define (mul-complex x y)
  (make-from-mag-ang
    (* (magnitude x) (magnitude y))
    (+ (angle x) (angle y))))
(define (div-complex x y)
  (make-from-mag-ang
    (/ (magnitude x) (magnitude y))
    (- (angle x) (angle y))))

;; セレクタ
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle     z))

;; コンストラクタ
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang   'polar) x y))

;; これはテスト
(define z1 (make-from-real-imag 4 3))
(real-part z1)
(imag-part z1)
(magnitude z1)

(define z2 (make-from-mag-ang 5 (atan (/ 3 4))))
(real-part z2)
(imag-part z2)
(magnitude z2)

(define z3 (add-complex z1 z2))
(real-part z3)
(imag-part z3)
(magnitude z3)

;;;;;;;;;;;;;;;;;;;;;;;;

; 14. 複素数パッケージを定義する
;     実態は直交形式パッケージと極形式パッケージを統合したパッケージ
(define (install-complex-package)
  ;; 直交形式パッケージと極形式パッケージからインポートする
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang   'polar) r a))

  ;; 内部手続き
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

  ;; ex2-80
  (define (=zero?-complex z)
    (and
      (= 0 (real-part z))
      (= 0 (imag-part z))))


  ;; インターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  ;; ex2-80
  (put '=zero? '(complex)      (lambda (z)          (=zero?-complex z)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'complex
       (lambda (x y) (tag (make-from-mag-ang   x y))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; 15.複素数パッケージをインストール
(install-complex-package)

; 16.各パッケージを利用できる.

(add (make-scheme-number 6) (make-scheme-number 3))
(sub (make-scheme-number 6) (make-scheme-number 3))
(mul (make-scheme-number 6) (make-scheme-number 3))
(div (make-scheme-number 6) (make-scheme-number 3))

(add (make-rational 2 3) (make-rational 1 2))
(sub (make-rational 2 3) (make-rational 1 2))
(mul (make-rational 2 3) (make-rational 1 2))
(div (make-rational 2 3) (make-rational 1 2))

(add (make-complex-from-real-imag 4 3) (make-complex-from-mag-ang 5 (atan (/ 3 4))))
(sub (make-complex-from-real-imag 4 3) (make-complex-from-mag-ang 5 (atan (/ 3 4))))
(mul (make-complex-from-real-imag 4 3) (make-complex-from-mag-ang 5 (atan (/ 3 4))))
(div (make-complex-from-real-imag 4 3) (make-complex-from-mag-ang 5 (atan (/ 3 4))))
;; ex2-80
(=zero? (make-complex-from-real-imag 4 3))
(=zero? (make-complex-from-real-imag 0 0))

; ポイント
; 各パケージが加法的に追加できる

; テスト
(define z1 (make-complex-from-real-imag 4 3))
(define z2 (make-complex-from-mag-ang 5 (atan (/ 3 4))))
