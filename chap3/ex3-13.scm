(define (make-cycle x)
  (set-cdr! (last-pair x) x))

(define z (make-cycle (list 'a 'b 'c)))
z

; z
; | a | b | c |  +  |
;   ^            |
;   |            |
;   +-------+----+
;
;
; 評価してもnullがないので表示無限ループとなる
