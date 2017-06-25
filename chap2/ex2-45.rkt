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
(define wave tri)
; (define wave stick-man)
; (define wave einstein)
;===============================================================================
; 前提の学習
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))
(define square-limit
  (square-of-four flip-horiz identity rotate180 flip-vert))
;===============================================================================
(define (split direction divider)
  (define (split-rec painter n)
    (if (= 0 n)
      painter
      (let ((smaller (split-rec painter (- n 1))))
        (direction painter (divider smaller smaller)))))
  split-rec)
(define right-split (split beside below))
(define up-split    (split below  beside))

;===============================================================================
; テスト
(paint (right-split wave 4))
(paint (up-split wave 4))
