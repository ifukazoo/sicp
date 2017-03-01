(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op result sequence)
  (if (null? sequence)
    result
    (op (car sequence)
        (accumulate op result (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; n以下の異なる正の整数が大小順に並んだ3リストを生成する
(define (unique-trio n)
  (flatmap
    (lambda (i)
      (flatmap
        (lambda (j)
             (map
               (lambda (k) (list i j k))
                  (enumerate-interval 1 (- j 1))))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))

(define (sum-trio t)
  (+ (car t) (car (cdr t)) (car (cdr (cdr t)))))

(define (match-sum-trios n s)
  (filter
    (lambda (t) (= s (sum-trio t)))
  (unique-trio n)))

; test
(match-sum-trios 10 9)

;備考
; unique-trioはこう書くとわかりやすいかも.
; (define (unique-trio n)
;   (define (f i)
;     (define (g j)
;       (define (h k) (list i j k))
;       (map h (enumerate-interval 1 (- j 1))))
;     (flatmap g (enumerate-interval 1 (- i 1))))
;   (flatmap f (enumerate-interval 1 n)))
