;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 前提の定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 足し算
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum (+ (get-value a1) (get-value a2)) me))
      ((and (has-value? a1) (has-value? sum))
       (set-value! a2 (- (get-value sum) (get-value a1)) me))
      ((and (has-value? a2) (has-value? sum))
       (set-value! a1 (- (get-value sum) (get-value a2)) me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

; 引き算
(define (subtracter a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum (- (get-value a1) (get-value a2)) me))
      ((and (has-value? a1) (has-value? sum))
       (set-value! a2 (+ (get-value sum) (get-value a1)) me))
      ((and (has-value? a2) (has-value? sum))
       (set-value! a1 (+ (get-value sum) (get-value a2)) me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: SUBTRACTER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

; かけ算
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      ((or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value! product (* (get-value m1) (get-value m2)) me))
      ((and (has-value? m1) (has-value? product))
       (set-value! m2 (/ (get-value product) (get-value m1)) me))
      ((and (has-value? m2) (has-value? product))
       (set-value! m1 (/ (get-value product) (get-value m2)) me))))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

; 割り算
(define (divider dividend divisor quotient)
  (define (process-new-value)
    (cond
      ((and (has-value? divisor) (= (get-value divisor) 0))
       (error "Divide by zero: DEVIDER" request))
      ((and (has-value? dividend) (has-value? divisor))
       (set-value! quotient (/ (get-value dividend) (get-value divisor)) me))
      ((and (has-value? divisor) (has-value? quotient))
       (set-value! dividend (* (get-value quotient) (get-value divisor)) me))
      ((and (has-value? quotient) (has-value? dividend))
       (set-value! divisor (/ (get-value dividend) (get-value quotient)) me))))

  (define (process-forget-value)
    (forget-value! dividend me)
    (forget-value! divisor me)
    (forget-value! quotient me)
    (process-new-value))

  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: DEVIDER" request))))
  (connect dividend me)
  (connect divisor me)
  (connect quotient me)
  me)

;定数
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request)) ;requestは受け付けない
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint) (constraint 'I-have-a-value))
(define (inform-about-no-value constraint) (constraint 'I-lost-my-value))

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name) (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter inform-about-value constraints))
        ((not (= value newval))
         (error "Contradiction" (list value newval)))
        (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant #f)
          (for-each-except retractor inform-about-no-value constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant #t #f))
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure lst)
  (define (loop items)
    (cond
      ((null? items) 'done)
      ((eq? (car items) exception) (loop (cdr items)))
      (else (procedure (car items))
            (loop (cdr items)))))
  (loop lst))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector newval informant) ((connector 'set-value!) newval informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))

; 練習問題 3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c- x y)
  (let ((z (make-connector)))
    (subtracter x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (divider x y z)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9.0) (cv 5.0))
          x)
      (cv 32.0)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))


; テスト
(probe "C=" C)
(probe "F=" F)

(set-value! C 100 'me)
(forget-value! C 'me)
(set-value! C 0 'me)
(forget-value! C 'me)

(set-value! F 20 'me)
(forget-value! F 'me)
