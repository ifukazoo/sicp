(define (make-account balance secret-password)
  (define password-list '())
  (define (register-password password)
    (set! password-list (cons password password-list)))
  (define (auth password lst)
    (cond
      ((null? lst) #f)
      ((eq? password (car lst)) #t)
      (else (auth password (cdr lst))))
    )
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance
        )
      "Insufficient funds"))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance
      ))
  (define (incorrect-password amount)
    "Incorrect password")
  (begin
    (register-password secret-password)
    (lambda (password arg)
      (if (auth password password-list)
        (cond
          ((eq? arg 'withdraw) withdraw)
          ((eq? arg 'deposit)  deposit)
          (else (register-password arg)))
        incorrect-password
        ))))

(define (make-joint account password new-password)
  (account password new-password))

;テスト
(define acc (make-account 100 'hogehoge))
((acc 'hogehoge 'withdraw) 50)
((acc 'hogehoge 'deposit)  150)
((acc 'fugafuga 'withdraw) 200)
(make-joint acc 'hogehoge 'fugafuga)
((acc 'fugafuga 'withdraw) 200)

