; 順序なしリストの実装
; (define (lookup given-key set-of-records)
;   (cond
;     ((null? set-of-records) #f)
;     ((equal given-key (key (car set-of-records))) (car set-of-records))
;     (else (lookup given-key (cdr set-of-records)))))

; 2分木の構造
(define (lookup given-key set-of-records)
  (cond
    ((null? set-of-records) #f)
    ((= given-key (key (entry set-of-records))) (value (entry set-of-records)))
    ((< given-key (key (entry set-of-records))) (lookup given-key  (left-branch set-of-records)))
    (else                                       (lookup given-key (right-branch set-of-records)))
    ))

;===============================================================================
; テスト
(define (make-record key value)
  (cons key value))
(define (key record)
  (car record))
(define (value record)
  (cdr record))

(define (make-tree entry left right)
  (list entry left right))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (entry tree)
  (car tree))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

; 例の木を作る
(define sample-sets (list->tree
                      (list
                        (make-record 4  'あ)
                        (make-record 5  'い)
                        (make-record 7  'う)
                        (make-record 8  'え)
                        (make-record 9  'お)
                        (make-record 12 'か)
                        (make-record 15 'き)
                        (make-record 16 'く)
                        (make-record 22 'け)
                        (make-record 26 'こ))))
(lookup  5 sample-sets)
(lookup 16 sample-sets)
(lookup 30 sample-sets)

