; バランス木を順序リストに変換する.
; リストの状態で和集合と共通集合を作る.
; 木に戻す.
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

(define (tree->list tree)
  (define (helper tree result)
    (if (null? tree)
      result
      (helper (left-branch tree)
              (cons (entry tree) (helper (right-branch tree) result)))))
  (helper tree '()))

(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x (car set1)) (y (car set2)))
      (cond
        ((= x y) (cons x (intersection-list (cdr set1) (cdr set2))))
        ((< x y)         (intersection-list (cdr set1) set2))
        (else            (intersection-list (cdr set2) set1)) ;(< y x)
        ))))
(define (union-list set1 set2)
  (cond
    ((and (null? set1) (null? set2)) '())
    ((null? set1)  set2)
    ((null? set2)  set1)
    (else
      (let ((x (car set1)) (y (car set2)))
        (cond
          ((< x y) (cons x (union-list (cdr set1) set2)))
          ((= x y) (cons x (union-list (cdr set1) (cdr set2))))
          (else    (cons y (union-list set1 (cdr set2)))) ;(> x y)
          )))))

(define (intersection-set tree1  tree2)
  (list->tree (intersection-list (tree->list tree1) (tree->list tree2))))
(define (union-set tree1 tree2)
  (list->tree (union-list (tree->list tree1) (tree->list tree2))))

(define t1 (list->tree '(4 5 7 8 9 12 15 16 22 26 27 28 31 35 36 39 42 46 52 56 57 68 72 75 76 86 94 95 99)))
(define t2 (list->tree '(1 3 4 5 6 9 11 13 16 18 19 21 24 28 29 30 32 34 36 38 41 43 45 47 48 49 53 53 55)))
(intersection-set t1 t2)
(union-set t1 t2)
