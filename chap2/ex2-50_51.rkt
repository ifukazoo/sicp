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

(define (segments->painter segments-list)
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

;waveペインタ
(define wave
  (segments->painter
    (list
      (make-segment (make-vect 0.5  0.4)
                    (make-vect 0.6  0))
      (make-segment (make-vect 0.5  0.4)
                    (make-vect 0.4  0))
      (make-segment (make-vect 0.3  0)
                    (make-vect 0.35 0.4))
      (make-segment (make-vect 0.35 0.4)
                    (make-vect 0.3  0.7))
      (make-segment (make-vect 0.3  0.7)
                    (make-vect 0.2  0.6))
      (make-segment (make-vect 0.2  0.6)
                    (make-vect 0    0.8))
      (make-segment (make-vect 0    0.9)
                    (make-vect 0.2  0.7))
      (make-segment (make-vect 0.2  0.7)
                    (make-vect 0.3  0.75))
      (make-segment (make-vect 0.3  0.75)
                    (make-vect 0.4  0.75))
      (make-segment (make-vect 0.4  0.75)
                    (make-vect 0.35 0.9))
      (make-segment (make-vect 0.35 0.9)
                    (make-vect 0.4  1))
      (make-segment (make-vect 0.5  1)
                    (make-vect 0.55 0.9))
      (make-segment (make-vect 0.55 0.9)
                    (make-vect 0.5  0.75))
      (make-segment (make-vect 0.5  0.75)
                    (make-vect 0.6  0.75))
      (make-segment (make-vect 0.6  0.75)
                    (make-vect 1    0.45))
      (make-segment (make-vect 1    0.35)
                    (make-vect 0.6  0.65))
      (make-segment (make-vect 0.6  0.65)
                    (make-vect 0.7  0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; ポイント
; 枠を求める手続きであるpainterに対し,
; 適用する枠自体を操作すれば,元の手続き(painter)のことを考える必要が
; なくなるという抽象化の実演である..

; transform-painter の役割
; 引数 => 新しい単位正方形の3つのベクトル
; 1)新単位正方形の原点を枠の値に変更.
; 2)新単位正方形の辺を枠の値に変更し,原点を調整している.
;   原点ベクトルを減算するということは,
;   図形的には原点ベクトルを(0,0)とした場合の座標に
;   変換していることになる.
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((mapper (frame-coord-map frame)))
      (let ((new-origin (mapper origin)))
        (let ((new-corner1 (sub-vect (mapper corner1) new-origin))
              (new-corner2 (sub-vect (mapper corner2) new-origin)))
          (painter (make-frame new-origin new-corner1 new-corner2)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (lambda (frame)
    (let ((left (transform-painter painter1
                                   (make-vect 0.0 0.0)
                                   (make-vect 0.5 0.0)
                                   (make-vect 0.0 1.0)
                                   ))
          (right (transform-painter painter2
                                    (make-vect 0.5 0.0)
                                    (make-vect 1.0 0.0)
                                    (make-vect 0.5 1.0)
                                    )))
      (left frame)
      (right frame))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (below1 painter1 painter2)
  (lambda (frame)
    (let ((bottom (transform-painter painter1
                                   (make-vect 0.0 0.0)
                                   (make-vect 1.0 0.0)
                                   (make-vect 0.0 0.5)
                                   ))
          (upper (transform-painter painter2
                                    (make-vect 0.0 0.5)
                                    (make-vect 1.0 0.5)
                                    (make-vect 0.0 1.0)
                                    )))
      (upper frame)
      (bottom frame))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
(define below below2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テスト
; (paint (flip-vert wave))
; (paint (shrink-to-upper-right wave))
; (paint (rotate90 wave))
; (paint (beside wave wave))
; (paint (flip-horiz wave))
; (paint (rotate180  wave))
; (paint (rotate270  wave))
(paint (below (rotate180 wave) wave))
