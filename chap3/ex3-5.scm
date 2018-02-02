(use srfi-27)
(define (rand n) (* (random-real) n))
(define (random-in-range low high)
  (+ (rand (- high low)) low))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0)
       (/ trials-passed trials))
      ((experiment)
       (iter (- trials-remaining 1)
             (+ trials-passed 1)))
      (else
        (iter (- trials-remaining 1)
              trials-passed))))
  (iter trials 0))

; モンテカルロ積分
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2)) (y (random-in-range y1 y2)))
      (p x y)))
  (let ((percentage-within (monte-carlo trials experiment))
        (rectangle-area (* (- x2 x1) (- y2 y1))))
    (* rectangle-area percentage-within)))

; PIの算出
(define (estimate-pi)
  (define (test-in-circle x y)
    ; 中心 (0.5,0.5) 半径0.5の円
    (<= (+ (square (- x 0.5)) (square (- y 0.5))) (square 0.5)))
  ; 円の面積
  (let ((area (estimate-integral test-in-circle 0 1.0 0 1.0 1000000)))
    (/ area (square 0.5))))

; テスト
(estimate-pi)
