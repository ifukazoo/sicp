(define (make-account balance secret-password)
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
  (lambda (password method)
    (if
      (eq? password secret-password)
      (cond
        ((eq? method 'withdraw) withdraw)
        ((eq? method 'deposit)  deposit)
        (else "no methods."))
      incorrect-password
      )))


;テスト
(define acc (make-account 100 'hogehoge))
((acc 'hogehoge 'withdraw) 50)
((acc 'hogehoge 'deposit)  150)
((acc 'fugafuga 'withdraw) 200)
