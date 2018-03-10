(define (factorical n)
  (if (= n 1)
    1
    (* n (factorical (- n 1)))))

(define (fact-iter product counter max-count)
  (if (< max-count counter)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
(define (factorical n)
  (fact-iter 1 1 n))

(factorical 10)
(factorical 1)
