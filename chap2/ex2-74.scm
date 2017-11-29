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
; 事業所Aシステム

; 事業所ローカルファイル
(define officeA-db-local '())

; 事業所内で実施すること
((lambda ()

   ; コンストラクタ
   ;   単に名前,住所,給料をリスト
   (define (make-record name address salary)
     (list name address salary))
   ; セレクタ
   (define (get-name record) (car record))
   (define (get-address record) (cadr record))
   (define (get-salary record) (caddr record))

   ;データベースに登録
   ;  単に従業員をリスト連結
   (define (register record lst)
     (append lst (list record)))

   ; データベースから取得
   (define (get-record employee db)
     (cond
       ((null? db) #f)
       ((eq? employee (get-name (car db))) (car db))
       (else (get-record employee (cdr db)))))

   ;データベース作成
   (set! officeA-db-local
     (register
       (make-record 'Debussy 'France 8000)
       (register
         (make-record 'Mozart 'Austria 9000)
         (register
           (make-record 'Bach 'Germany 10000) officeA-db-local))))

   ;事業所内でのデータ取得
   (get-record 'Mozart officeA-db-local)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 事業所Bシステム

; 事業所ローカルファイル
(define officeB-db-local (cons 0 '()))

; 事業所内で実施すること
((lambda ()

   ; コンストラクタ
   ;   名前,住所,給料にタイトルを付けてリスト化する
   (define (make-record name address salary)
     (list (cons 'name name) (cons 'address address) (cons 'salary salary)))
   ; セレクタ
   (define (get-val record tag)
     (if (eq? tag (car (car record)))
       (cdr (car record))
       (get-val (cdr record) tag)))
   (define (get-name record)
     (get-val record 'name))
   (define (get-address record)
     (get-val record 'address))
   (define (get-salary record)
     (get-val record 'salary))

   ;データベースに登録
   ;  データベースに項番を付けてみる.
   ;  ※あくまでも実用上の必要ではなく,事業所Aとの違いをつけるため.
   (define (register record index lst)
     (cons (+ index 1) (append lst (list (cons index record)))))
   (define (get-record employee db)
     (define (get-record-from-table table)
       (if (null? table)
         #f
         (let ((record (cdr (car table))))
           (if (eq? employee (get-name record))
             record
             (get-record-from-table (cdr table))))))
     (get-record-from-table (cdr db)))

   ;データベース作成
   (set! officeB-db-local
     (register (make-record 'Beethoven 'Germany 11000)
               (car officeB-db-local) (cdr officeB-db-local)))
   (set! officeB-db-local
     (register (make-record 'Chopin 'Polland 6000)
               (car officeB-db-local) (cdr officeB-db-local)))
   (set! officeB-db-local
     (register (make-record 'R.Strauss 'Austria 7000)
               (car officeB-db-local) (cdr officeB-db-local)))

   ;事業所内でのデータ取得
   (get-record 'Chopin officeB-db-local)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 全社システム

; 事業所ローカルのデータベースを全社システム向けにタグをつける.
(define (convert-local-db office-name local-db)
  (cons office-name local-db))

; セレクタ
(define (office-name filename)
  (car filename))
(define (file filename)
  (cdr filename))

; a. get-record
(define (get-record employee filename)
  ((get 'get-record (office-name filename)) employee (file filename)))

; b. get-salary
(define (get-salary office-name record)
  ((get 'get-salary office-name) record))

; c. find-employee-record
(define (find-employee-record employee lst)
  (if (null? lst)
    #f
    (let ((record (get-record employee (car lst))))
      (if record
        record
        (find-employee-record employee (cdr lst))))))

; 事業所Aを全社システムに組み込む
(define (install-officeA-package)
  (define (get-record employee db)
    (cond
      ((null? db) #f)
      ((eq? employee (get-name (car db))) (car db))
      (else (get-record employee (cdr db)))))
  (define (get-name record) (car record))
  (define (get-address record) (cadr record))
  (define (get-salary record) (caddr record))
  (define (get-val record tag)
    (if (eq? tag (car (car record)))
      (cdr (car record))
      (get-val (cdr record) tag)))
  ; ; システムの他の部分とのインターフェース
  (put 'get-record 'officeA get-record)
  (put 'get-salary 'officeA get-salary)
  'done)
(install-officeA-package)
(define officeA-db '())
(set! officeA-db (convert-local-db 'officeA officeA-db-local))

; 事業所Bを全社システムに組み込む
(define (install-officeB-package)
  (define (get-val record tag)
    (if (eq? tag (car (car record)))
      (cdr (car record))
      (get-val (cdr record) tag)))
  (define (get-name record)
    (get-val record 'name))
  (define (get-address record)
    (get-val record 'address))
  (define (get-salary record)
    (get-val record 'salary))
  (define (get-record employee db)
    (define (get-record-from-table table)
      (if (null? table)
        #f
        (let ((record (cdr (car table))))
          (if (eq? employee (get-name record))
            record
            (get-record-from-table (cdr table))))))
    (get-record-from-table (cdr db)))

  ; ; システムの他の部分とのインターフェース
  (put 'get-record 'officeB get-record)
  (put 'get-salary 'officeB get-salary)
  'done)
(install-officeB-package)
(define officeB-db '())
(set! officeB-db (convert-local-db 'officeB officeB-db-local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト
(get-record 'Mozart officeA-db)
(get-record 'Chopin officeB-db)
(get-salary 'officeA (get-record 'Mozart officeA-db))
(get-salary 'officeB (get-record 'Chopin officeB-db))
(find-employee-record 'Chopin (list officeA-db officeB-db))
(find-employee-record 'Mozart (list officeA-db officeB-db))
(find-employee-record 'Brahms (list officeA-db officeB-db))
