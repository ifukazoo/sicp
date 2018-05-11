; 復習

; ;
; ; 一次元
; ;
;
; ; テーブル定義
; (define (make-table)
;   (list '*table*))
;
; ; キー => レコード
; (define (assoc key records)
;   (cond
;     ((null? records) #f)
;     ((equal? key (caar records)) (car records))
;     (else (assoc key (cdr records)))))
;
; ; キー => 値
; (define (lookup key table)
;   (let ((record (assoc key (cdr table))))
;     (if record
;       (cdr record)
;       #f)))
;
; (define (insert! key value table)
;   (let ((record (assoc key (cdr table))))
;     (if record
;       (set-cdr! record value)
;       (set-cdr! table (cons (cons key value) (cdr table))))))
;
; ; テスト
; (define my-records (make-table))
; (insert! 'Mozart    "Le nozze di Figaro" my-records)
; (insert! 'Bach      "chaconne"           my-records)
; (insert! 'Beethoven "Eroica"             my-records)
;
; (lookup 'Mozart    my-records)
; (lookup 'Bach      my-records)
; (lookup 'Beethoven my-records)
; (lookup 'Brahms    my-records)

; ;
; ; 一次元 手続き形式
; ;
;
; (define (make-table)
;   (define table (list '*table*))
;   (define (assoc key records)
;     (cond
;       ((null? records) #f)
;       ((equal? key (caar records)) (car records))
;       (else (assoc key (cdr records)))))
;   ; キー => 値
;   (define (lookup key)
;     (let ((record (assoc key (cdr table))))
;       (if record
;         (cdr record)
;         #f)))
;   (define (insert! key value)
;     (let ((record (assoc key (cdr table))))
;       (if record
;         (set-cdr! record value)
;         (set-cdr! table (cons (cons key value) (cdr table))))))
;   (define (dispatch m)
;     (cond
;       ((equal? m 'lookup)  lookup)
;       ((equal? m 'insert!) insert!)
;       (else (error "no method " m))))
;   dispatch)
; (define my-records (make-table))
; (define (lookup key table) ((table 'lookup) key))
; (define (insert! key value table) ((table 'insert!) key value))
;
; (insert! 'Mozart    "Le nozze di Figaro" my-records)
; (insert! 'Bach      "chaconne"           my-records)
; (insert! 'Beethoven "Eroica"             my-records)
;
; (lookup 'Mozart    my-records)
; (lookup 'Bach      my-records)
; (lookup 'Beethoven my-records)
; (lookup 'Brahms    my-records)

; ;
; ; 2次元 手続き形式
; ;

; 2018/04/25 06:26 中断

(define (make-table)
  (list '*table*))

(define (assoc key records)
  (cond
    ((null? records) #f)
    ((equal? key (caar records)) (car records))
    (else (assoc key (cdr records)))))

(define (lookup key1 key2 table)
  (let ((subtable (assoc key1 table)))
    (if subtable
      (let ((record (assoc key2 subtable)))
        (if record
          (cdr record)
          #f)
        )
      #f)
    ))

(define (insert! key1 key2 value table)
  (let ((subtable (assoc key1 table)))
    (if subtable
      ((let ((record (assoc key2 subtable)))
         (if record
           (set-cdr! record value)
           (set-cdr! subtable (cons (cons key2 value) subtable))
           )))
      (let ((new-subtable (list (cons key2 value))))
        (set-cdr! table (cons subtable new-subtable)))
      )))

(define t (make-table))
(insert! 'Germany 'Mozart "Le nozze di Figaro" t)


(list 1 2 3)
(cdr (list 1 2 3))
(cdr (cdr (list 1 2 3)))
(cdr (cdr (cdr (list 1 2 3))))
