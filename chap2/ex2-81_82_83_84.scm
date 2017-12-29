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

;;; 型変換テーブルインターフェース
(define type-conversion-table '())
(define (get-coercion from to)
  (let ((result (get-kv (get-kv type-conversion-table from) to)))
    (if (null? result) #f result)))
(define (put-coercion from to item)
  (set! type-conversion-table
    (put-kv type-conversion-table from (put-kv (get-kv type-conversion-table from) to item))))

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
        (if (= (length args) 2) ;引数が2つの場合に型変換を試す
          (let ((type1 (car  type-tags))
                (type2 (cadr type-tags))
                (arg1  (car  args))
                (arg2  (cadr args)))
            ; 19. 練習問題 2.81 c.
            (if (not (eq? type1 type2))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond
                  (t1->t2 (apply-generic op (t1->t2 arg1) arg2))
                  (t2->t1 (apply-generic op arg1 (t2->t1 arg2)))
                  (error "No method for these types : APPLY-GENERIC "
                         (list op type-tags))))
              (error "No method for these types : APPLY-GENERIC "
                     (list op type-tags))))
          (error "No method for these types : APPLY-GENERIC "
                 (list op type-tags))))
      )))

; 1.汎用性のある四則演算を定義する
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; 2.通常の数字パッケージを定義する.
; 練習問題 2.83
;   scheme-numberは実数パッケージとする
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  ; 練習問題 2.81 a.
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))

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


  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))


  (put 'numer '(rational)        (lambda (x)   (numer x )))
  (put 'denom '(rational)        (lambda (x)   (denom x )))

  (put 'make 'rational           (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))



; 練習問題 2.83
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

; 7.有理数パッケージをインストール
(install-rational-number-package)

; 8.有理数パッケージを使用する
(add (make-rational 2 3) (make-rational 1 2))
(sub (make-rational 2 3) (make-rational 1 2))
(mul (make-rational 2 3) (make-rational 1 2))
(div (make-rational 2 3) (make-rational 1 2))

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

  ;; インターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
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

; ポイント
; 各パケージが加法的に追加できる

; 17. 強制型変換
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

; テスト
(define n1 (make-scheme-number 5))
(add n1 z1)
(add n1 z2)

; 18. 練習問題 2.81
; 引数が同じであっても強制型変換をする処理を入れてみる.
(define (scheme-number->scheme-number n) n)
(define (complex->complex n) n)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex       'complex       complex->complex)

; 19. 練習問題 2.81 a.
; 永遠に型変換を行う無限ループとなる.
(define (exp x y) (apply-generic 'exp x y))
; テスト
(define s1 (make-scheme-number 2))
(define s2 (make-scheme-number 5))
(exp s1 s2)
(define z1 (make-complex-from-real-imag 4 3))
(define z2 (make-complex-from-mag-ang 5 (atan (/ 3 4))))
; (exp z1 z2)

; 練習問題 2.82
; A->D,B->D,C->Dの型変換を定義している場合に,A,B,C間の型変換だけを試みてもDに行き着かない.

; 練習問題 2.83
; 2.83 1)scheme-integerパッケージを入れる
(define (install-scheme-integer-package)
  (define (tag x) (attach-tag 'scheme-integer x))
  (put 'add '(scheme-integer scheme-integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-integer scheme-integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-integer scheme-integer) (lambda (x y) (tag (* x y))))
  ; "div"を上書きしてしまっているのでdiv0をその場しのぎで使う
  (put 'div '(scheme-integer scheme-integer) (lambda (x y) (tag (div0 x y))))
  (put 'make 'scheme-integer                 (lambda (x)   (tag x)))
  'done)
(define (make-scheme-integer n)
  ((get 'make 'scheme-integer) n))

(install-scheme-integer-package)

; 練習問題 2.83
; 整数   -> 有理数
(define (scheme-integer->rational i)
  (make-rational (contents i) 1))
(put-coercion 'scheme-integer 'rational scheme-integer->rational)

; 有理数 -> 実数
(define (rational->scheme-number i)
  (make-scheme-number (/ (numer i) (denom i))))
(put-coercion 'rational 'scheme-number rational->scheme-number)


; 練習問題 2.83
(define (raise base)
  (cond
    ((eq? (type-tag base) 'scheme-integer) ((get-coercion 'scheme-integer 'rational)      base))
    ((eq? (type-tag base) 'rational)       ((get-coercion 'rational       'scheme-number) base))
    ((eq? (type-tag base) 'scheme-number)  ((get-coercion 'scheme-number  'complex)       base))
    (else (error "tried to raise, but failed: APPLY-GENERIC "))))


; 練習問題 2.83
; 整数   -> 有理数
(define int1 (make-scheme-integer 6))
(define int2 (make-scheme-integer 2))
(raise int1)
(raise int2)
(numer (raise int2))
(denom (raise int2))

; 有理数 -> 実数
(define int3 (make-scheme-integer 7))
(define int4 (make-scheme-integer 3))
(define r1 (raise (raise int3)))
(define r2 (raise (raise int4)))


; 実数   -> 複素数
(define c1 (raise (raise (raise int3))))
(define c2 (raise (raise (raise int4))))

; 練習問題 2.84
; (型階層の高い方 型階層の低い方) の形式のリストを返す
(define (high-low x y)
  (define tower '(complex scheme-number rational scheme-integer))
  (define (iter-tower twr)
    (cond
      ((null? twr) '())
      ((eq? (car twr) (type-tag x)) (list x y))
      ((eq? (car twr) (type-tag y)) (list y x))
      (else (iter-tower (cdr twr)))))
  (iter-tower tower))
(define (higher highlow)
  (car highlow))
(define (lower highlow)
  (cadr highlow))

; apply-genericを再実装する
(define (raise-until-eq-type high-type low-type)
  (if (eq? (type-tag high-type) (type-tag low-type))
    low-type
    (raise-until-eq-type high-type (raise low-type))))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2) ;引数が2つの場合に型変換を試す
          (let ((arg1  (car  args))
                (arg2  (cadr args)))
            (let ((highlow (high-low arg1 arg2)))
              (if (not (null? highlow))
                (apply-generic op (higher highlow) (raise-until-eq-type (higher highlow) (lower highlow)))
                (error "No method for these types : APPLY-GENERIC "
                       (list op type-tags)))))
          (error "No method for these types : APPLY-GENERIC "
                 (list op type-tags))))
      )))

(define int3 (make-scheme-integer 5))
(define c3 (make-complex-from-real-imag 4 3))
(add int3 c3)
