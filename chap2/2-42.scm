; 既定の関数
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op result sequence)
  (if (null? sequence)
    result
    (op (car sequence)
        (accumulate op result (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 新しいQueenの配置を追加する.
(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define empty-board '())
(define (include? pos positions)
  (if (null? positions)
    #f
    (let ((it (car positions)))
      (if (and (= (car it) (car pos))
               (= (cadr it) (cadr pos)))
        #t
        (include? pos (cdr positions))))))

; 該当列のQueenを見つける.見つからないことは想定してない.
(define (find-pos k positions)
  (let ((it (car positions)))
    (if (= k (car it))
      it
      (find-pos k (cdr positions)))))

; 利き筋を作る.盤外となってしまう座標も含める.
(define (control-pos pos)
  (flatmap (lambda (col)
             (list
               (list col (- (cadr pos) (- (car pos) col)))   ; 北西方向
               (list col (cadr pos))                         ; 西方向
               (list col (+ (cadr pos) (- (car pos) col))))) ; 南西方向
  (enumerate-interval 1 (- (car pos) 1))))

(define (safe? k positions)
  (let ((controls (control-pos (find-pos k positions))))
    (accumulate (lambda (pos safe)
                  (if safe
                    (and safe (not (include? pos controls)))
                    safe))
                #t positions)))
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) ; new-row には 1,2,...の順で行番号が入る
                   (adjoin-position
                     new-row k rest-of-queens)) ; k=>新しい列
                 ;1-8を生成
                 (enumerate-interval 1 board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; テスト
(queens 4)
(queens 8)

