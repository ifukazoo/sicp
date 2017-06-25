#lang racket/gui

; 参考
; https://gist.github.com/etscrivner/e0105d9f608b00943a49

(require graphics/graphics)
(open-graphics)

;;;; 基本
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
    (* s (xcor-vect v))
    (* s (ycor-vect v))))

; ベクトルを渡すと,フレームサイズにスケールした新しいベクトルが出てくる.
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
        (scale-vect (xcor-vect v) (edge1-frame frame))
        (scale-vect (ycor-vect v) (edge2-frame frame))))))

; ライブラリ固有の実装
; graphics/graphics
; https://docs.racket-lang.org/graphics/Draw__Clear__and_Flip_Operations.html
; view-port枠と同じ座標が描画できるようにパラメータに少し下駄を履かせる
(define view-port (open-viewport "A Picture Language" 520 520))

(define (draw-line_ start end)
  ((draw-line view-port)
   (make-posn
     (+ 10 (xcor-vect start))
     (+ 10 (ycor-vect start)))
   (make-posn
     (+ 10 (xcor-vect end))
     (+ 10 (ycor-vect end)))))

(define (segments-painter segments-list)
    (lambda (frame)
      (for-each (lambda (segment)
                  (draw-line_
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
                segments-list)))

(define (paint painter)
  (painter
    (make-frame
      (make-vect 0 0)
      (make-vect 500 0)
      (make-vect 0 500))))
; painterはframeを渡すと描画を行う手続き型とみなす.

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト

(define v1 (make-vect 20 30))
(define v2 (make-vect 120 230))
(define v3 (make-vect 0.0 0.0))
(define v4 (make-vect 0.2 0.6))
(define frame1
  (make-frame
    (make-vect 0 0)
    (make-vect 200 0)
    (make-vect 0 200)))

(define seg1
  (make-segment
    (make-vect 0 0)
    (make-vect 1 0)))
(define seg2
  (make-segment
    (make-vect 0 0)
    (make-vect 0 1)))
(define seg3
  (make-segment
    (make-vect 1 0)
    (make-vect 1 1)))
(define seg4
  (make-segment
    (make-vect 0 1)
    (make-vect 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 問題
; a. 指定された枠の輪郭を描くペインタ
(define (outline-painter frame)
  (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame  frame)))
    (let ((edge3 (sub-vect (add-vect edge1 edge2) origin)))
      (segments-painter (list
                          (make-segment origin edge1)
                          (make-segment origin edge2)
                          (make-segment edge1  edge3)
                          (make-segment edge2  edge3))))))

; b. 枠の対角線同士をつないで"X"を描くペインタ
(define (x-painter frame)
  (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame  frame)))
    (let ((edge3 (sub-vect (add-vect edge1 edge2) origin)))
      (segments-painter (list
                          (make-segment origin edge3)
                          (make-segment edge1 edge2))))))

; c. 指定された枠の中点をつないでひし形を描くペインタ
(define (rhombus-painter frame)
  (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame  frame)))
    (let ((edge3 (sub-vect (add-vect edge1 edge2) origin)))
      (let ((m0 (scale-vect 0.5 edge1))
            (m1 (add-vect edge1 (scale-vect 0.5 edge2)))
            (m2 (add-vect edge2 (scale-vect 0.5 edge1)))
            (m3 (scale-vect 0.5 edge2)))
      (segments-painter (list
                          (make-segment m0 m1)
                          (make-segment m1 m2)
                          (make-segment m2 m3)
                          (make-segment m3 m0)))))))

; d. waveペインタ
(define wave (segments-painter (list
                      (make-segment (make-vect 0.5 0.3) (make-vect 0.5 0.9))
                      (make-segment (make-vect 0.2 0.1) (make-vect 0.5 0.3))
                      (make-segment (make-vect 0.8 0.1) (make-vect 0.5 0.3))
                      (make-segment (make-vect 0.25 0.65) (make-vect 0.75 0.65))
                      (make-segment (make-vect 0.25 0.65) (make-vect 0.2 0.8))
                      (make-segment (make-vect 0.75 0.65) (make-vect 0.8 0.5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テスト
; a. 指定された枠の輪郭を描くペインタ
; (paint (outline-painter (make-frame
;                           (make-vect 0.0 0.0)
;                           (make-vect 1.0 0.0)
;                           (make-vect 0.0 1.0))))

; b. 指定された枠の輪郭を描くペインタ
; (paint (x-painter (make-frame
;                           (make-vect 0.0 0.0)
;                           (make-vect 1.0 0.0)
;                           (make-vect 0.0 1.0))))

; c. 指定された枠の中点をつないでひし形を描くペインタ
; (paint (rhombus-painter (make-frame
;                           (make-vect 0.0 0.0)
;                           (make-vect 1.0 0.0)
;                           (make-vect 0.0 1.0))))

; d. waveペインタ
(paint wave)
