(define (make-f)
  (let ((zero-evaled #f))
    (lambda (arg)
      (let ((value (if zero-evaled 0 arg)))
        (begin
          (set! zero-evaled (zero? arg))
          value)))))

(define f1 (make-f))
(f1 1)
(f1 0)

(define f2 (make-f))
(f2 0)
(f2 1)
