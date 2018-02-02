(define (make-account balance secret-password)
  (define count-incorrect-input 0)
  (define limit-incorrect-input 7)
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
    (begin
      (set! count-incorrect-input (+ count-incorrect-input 1))
      (if (> limit-incorrect-input count-incorrect-input)
        "Incorrect password"
        (call-the-cops)
        )))
  (define (call-the-cops)
    "A policeman will come soon")
  (lambda (password method)
    (if
      (eq? password secret-password)
      (cond
        ((eq? method 'withdraw) withdraw)
        ((eq? method 'deposit)  deposit)
        (else "no methods."))
      incorrect-password)
    ))


;テスト
(define acc (make-account 100 'hogehoge))
((acc 'hogehoge 'withdraw) 50)
((acc 'hogehoge 'deposit)  150)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
((acc 'fugafuga 'withdraw) 200)
