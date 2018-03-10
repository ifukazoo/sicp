(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
; => (a b)
;
;   before append
;
;   x -> | a | b |
;   y -> | c | d |
; ------------
;   after append

;   x -> | a | b |
;   y -> | c | d |
;   z -> | a | b | c | d |
;

(define w (append! x y))
w
(cdr x)
; => (a b c d)
;
;   before append!
;
;   x -> | a | b |
;   y -> | c | d |
; ------------
;   after append!

;   x -> | a | b | c | d |
;   y -> | c | d |
;   w -> x
;
