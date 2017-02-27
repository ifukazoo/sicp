; 素数判定用
(define (divides? a b) (= (remainder a b) 0))
(define (find-divisor n test-divisor) (cond ((> (square test-divisor) n) n) ((divides? n test-divisor) test-divisor) (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n) (= (smallest-divisor n) n))

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

; unique-pairsの定義
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
         (enumerate-interval 2 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; prime-sum-pairsの再定義
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; test
(unique-pairs 5)
(prime-sum-pairs 5)
