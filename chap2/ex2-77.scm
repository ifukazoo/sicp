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


; 直交形式パッケージを定義
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
(install-rectangler-package)

; 極形式パッケージを定義
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
(install-polar-package)

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

; 複素数パッケージを定義する
(define (install-complex-package)
  ;; 直交形式パッケージと極形式パッケージからインポートする
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang   'polar) r a))
  ;; ex2-77ここに追加する
  (define (real-part z)
    ((get 'real-part '(rectangular)) z))
  (define (imag-part z)
    ((get 'imag-part '(rectangular)) z))
  (define (magnitude z)
    ((get 'magnitude '(polar)) z))
  (define (angle z)
    ((get 'angle '(polar)) z))

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

  ;; ex2-77ここに追加する
  (put 'real-part '(complex) (lambda (z) (real-part (contents z))))
  (put 'imag-part '(complex) (lambda (z) (imag-part (contents z))))
  (put 'magnitude '(complex) (lambda (z) (magnitude (contents z))))
  (put 'angle     '(complex) (lambda (z) (angle     (contents z))))

  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

; ポイント
; 各パケージが加法的に追加できる

; テスト
(define z1 (make-complex-from-real-imag 4 3))
(define z2 (make-complex-from-mag-ang 5 (atan (/ 3 4))))
(real-part z1)
(imag-part z1)
(magnitude z2)
(angle z2)
