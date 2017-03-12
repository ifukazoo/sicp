#lang racket
(require sicp-pict)

(define tri (segments->painter (list
                            (make-segment (make-vect 0.01 0.01) (make-vect 0.5 0.99))
                            (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.01))
                            (make-segment (make-vect 0.99 0.01) (make-vect 0.01 0.01))
                            )))
(define stick-man (segments->painter (list
                            (make-segment (make-vect 0.5 0.3) (make-vect 0.5 0.9))
                            (make-segment (make-vect 0.2 0.1) (make-vect 0.5 0.3))
                            (make-segment (make-vect 0.8 0.1) (make-vect 0.5 0.3))
                            (make-segment (make-vect 0.25 0.65) (make-vect 0.75 0.65))
                            (make-segment (make-vect 0.25 0.65) (make-vect 0.2 0.8))
                            (make-segment (make-vect 0.75 0.65) (make-vect 0.8 0.5))

                            ; (make-segment (make-vect 0.01 0.01) (make-vect 0.99 0.01))
                            ; (make-segment (make-vect 0.99 0.01) (make-vect 0.99 0.99))
                            ; (make-segment (make-vect 0.99 0.99) (make-vect 0.01 0.99))
                            ; (make-segment (make-vect 0.01 0.99) (make-vect 0.01 0.01))
                            )))
; (define wave tri)
(define wave stick-man)
; (define wave einstein)

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))
(define (right-split painter n)
  (if (= 0 n)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= 0 n)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= 0 n)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))
(paint (corner-split wave 4))
