;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; キューの実装

(define (make-queue)
  (define lst       '())
  (define front-ptr '())
  (define rear-ptr  '())
  (define (empty-queue?) (null? lst))
  (define (delete-queue)
    (if (empty-queue?)
      (error "DELETE! called with an empty queue")
      (begin
        (set! lst (cdr lst))
        (set! front-ptr lst)
        )))
  (define (insert-queue item)
    (let ((new-pair (cons item '())))
      (if (empty-queue?)
        (begin
          (set! lst new-pair)
          (set! front-ptr lst)
          (set! rear-ptr  lst)
          )
        (begin
          (set-cdr! rear-ptr new-pair)
          (set! rear-ptr new-pair)
          ))))
  (define (dispatch m)
    (cond
      ((eq? m 'insert-queue!) insert-queue)
      ((eq? m 'front-queue)   (car front-ptr))
      ((eq? m 'delete-queue!) delete-queue)
      ((eq? m 'empty-queue?) empty-queue?)
      ((eq? m 'print-queue)   lst)
      (else (error "no method call" m)))
    )
  dispatch)

(define (insert-queue! queue  item)
  ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  ((queue 'delete-queue!)))
(define (front-queue queue)
  (queue 'front-queue))
(define (empty-queue? queue)
  ((queue 'empty-queue?)))
(define (print-queue queue)
  (queue 'print-queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 予定表(agenda)のデータ構造
; agenda
;   car current-time
;   cdr (segment segment segment ...)

; segment
;   car time
;   cdr queue (action action action ...)

; segmentのコンストラクタとセレクタ
(define (make-time-segment tm queue) (cons tm queue))
(define (segment-time  s) (car s))
(define (segment-queue s) (cdr s))

; agendaのコンストラクタとセレクタ
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (segments     agenda) (cdr agenda))

; agendaのセッター
(define (set-current-time! agenda tm)       (set-car! agenda tm))
(define (set-segments!     agenda segments) (set-cdr! agenda segments))

; agendaセグメントのセレクタ
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segment  agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

; 予定表への追加
(define (add-to-agenda! tm action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< tm (segment-time (car segments)))))
  (define (make-new-time-segment tm action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment tm q)))
  (define (add-to-segments! segments)
    ; segment     segment segment ...
    ; ^       ^
    ; |       (2)先頭segmentの直後に新しいsegmentを作成して挿入するパターン
    ; (1)先頭segmentの最後に入れるパターン
    (if (= tm (segment-time (car segments)))
      ; then パターン(1)
      (insert-queue! (segment-queue (car segments)) action)
      ; else
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          ; then パターン(2)
          (set-cdr! segments (cons (make-new-time-segment tm action) rest))
          ; else
          (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      ; then 先頭にsegment作成
      (set-segments! agenda (cons (make-new-time-segment tm action) segments))
      ; else 適切な位置に挿入
      (add-to-segments! segments))))

; 予定表から削除
;   先頭のsegment-queue から要素を1つ削除する.
;   空であれば先頭のsegmentを付け替える.
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segment agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 環境変数 the-agenda を前提とした ユーティリティ
(define the-agenda (make-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda)) action the-agenda))

; ここまでのテスト
; (add-to-agenda! 1 (lambda () (display "hello china\n"))       the-agenda)
; (add-to-agenda! 3 (lambda () (display "hello france\n"))      the-agenda)
; (add-to-agenda! 1 (lambda () (display "hello japan\n"))       the-agenda)
; (add-to-agenda! 2 (lambda () (display "hello mexico\n"))      the-agenda)
; (add-to-agenda! 1 (lambda () (display "hello north-korea\n")) the-agenda)
; (add-to-agenda! 3 (lambda () (display "hello germany\n"))     the-agenda)
; (add-to-agenda! 1 (lambda () (display "hello south-korea\n")) the-agenda)
; (add-to-agenda! 2 (lambda () (display "hello usa\n"))         the-agenda)
; (add-to-agenda! 3 (lambda () (display "hello italy\n"))       the-agenda)
; (after-delay  3 (lambda () (display "hello japan\n")))
; (propagate)
; => OK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; wireの実装
(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (make-wire)
  (define signal-value      0)   ; 0
  (define action-procedures '()) ; 手続きなし

  (define (set-my-signal! new-value)
    (if (not (= signal-value new-value))
      (begin
        (set! signal-value new-value)
        (call-each action-procedures))
      'done))
  (define (accept-action-procedures! proc)
    (set! action-procedures (cons proc action-procedures))
    (proc))

  (define (dispatch m)
    (cond
      ((eq? m 'get-signal)  signal-value)
      ((eq? m 'set-signal!) set-my-signal!)
      ((eq? m 'add-action!) accept-action-procedures!)
      (else (error "Unknown operation: WIRE" m))))
  dispatch)
(define (get-signal  wire)                   (wire  'get-signal))
(define (set-signal! wire  new-value)        ((wire 'set-signal!) new-value))
(define (add-action! wire  action-procedure) ((wire 'add-action!) action-procedure))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 結線の実装

; 遅延定数
(define direct-delay 1)
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; 論理関数
(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))
(define (logical-not s)
  (cond
    ((= s 0) 1)
    ((= s 1) 0)
    (else (error "Invalid signal" s))))
(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 1 0))

; 直結回路
(define (direct input output)
  (define direct-input (lambda () (set-signal! output (get-signal input))))
  (add-action!
    input
    (lambda ()
      (after-delay direct-delay
                   direct-input)))
  'ok)

; ; 直結のテスト
; ;   2本線を定義して変化があったら表示
; (define input-1 (make-wire))
; (probe "input" input-1)
; (define output-1 (make-wire))
; (probe "output" output-1)
;
; ; 今の値
; (get-signal input-1)
; (get-signal output-1)
;
; ; 結線する
; (direct input-1 output-1)
; ; 値の変更する
; (set-signal! input-1 1)
; ; シミュレートする.
; (propagate)

; 否定回路

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

; ; 否定回路のテスト
; ;   2本線を定義して変化があったら表示
; (define input-1 (make-wire))
; (probe "input" input-1)
; (define output-1 (make-wire))
; (probe "output1" output-1)
;
; ; 今の値
; (get-signal input-1)
; (get-signal output-1)
;
; ; 結線する
; (inverter input-1 output-1)
;
; ; 値の変更する
; (set-signal! input-1 1)
; ; シミュレートする.
; (propagate)

; AND回路
(define (and-gate input1 input2 output)
  (define (and-input)
    (let ((new-value (logical-and (get-signal input1) (get-signal input2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input1 and-input)
  (add-action! input2 and-input)
  'ok)

; ; AND回路のテスト
; ;   2本線を定義して変化があったら表示
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define output-1 (make-wire))
; (probe "output1" output-1)
;
; ; 今の値
; (get-signal input-1)
; (get-signal input-2)
; (get-signal output-1)
;
; ; 結線する
; (and-gate input-1 input-2 output-1)
;
; ; 値を変更して確認する.
; (set-signal! input-1 1)
; (propagate)
;
; (set-signal! input-1 1)
; (set-signal! input-2 1)
; (propagate)
;
; (set-signal! input-2 0)
; (propagate)

; 練習問題 3.28
; OR回路
(define (or-gate input1 input2 output)
  (define (or-input)
    (let ((new-value (logical-or (get-signal input1) (get-signal input2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input1 or-input)
  (add-action! input2 or-input)
  'ok)

; 練習問題 3.29
; OR回路2
(define (or-gate2 input1 input2 output)
  (define (or-input)
    (let ((invert-input1 (logical-not (get-signal input1)))
          (invert-input2 (logical-not (get-signal input2))))
      (let ((merged-input (logical-and invert-input1 invert-input2)))
        (let ((new-value (logical-not merged-input)))
          (after-delay or-gate-delay
                       (lambda () (set-signal! output new-value)))))))

  (add-action! input1 or-input)
  (add-action! input2 or-input)
  'ok)

; テスト
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define output-1 (make-wire))
; (probe "output1" output-1)
;
; (get-signal input-1)
; (get-signal input-2)
; (get-signal output-1)
;
; (or-gate input-1 input-2 output-1)
;
; (set-signal! input-1 1)
; (propagate)
; (set-signal! input-1 0)
; (propagate)
; (set-signal! input-2 1)
; (propagate)
; (set-signal! input-2 0)
; (propagate)
;
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define output-1 (make-wire))
; (probe "output1" output-1)
;
; (get-signal input-1)
; (get-signal input-2)
; (get-signal output-1)
;
; (or-gate2 input-1 input-2 output-1)
;
; (set-signal! input-1 1)
; (propagate)
; (set-signal! input-1 0)
; (propagate)
; (set-signal! input-2 1)
; (propagate)
; (set-signal! input-2 0)
; (propagate)

; 半加算器
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; テスト
; (define a (make-wire))
; (define b (make-wire))
; (define s (make-wire))
; (define c (make-wire))
; (probe "s" s)
; (probe "c" c)
; (half-adder a b s c)
;
; (set-signal! a 0)
; (set-signal! b 1)
; (propagate)

; 全加算器
(define (full-adder a b c-in sum c-out)
  (let ((s1 (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s1 c1)
    (half-adder a s1 sum c2)
    (or-gate c2 c1 c-out)
    'ok))

; テスト
; (define a (make-wire))
; (define b (make-wire))
; (define c-in (make-wire))
; (define sum (make-wire))
; (define c-out (make-wire))
; (probe "sum" sum)
; (probe "c-out" c-out)
; (full-adder a b c-in sum c-out)
;
; (set-signal! a    0)
; (set-signal! b    0)
; (set-signal! c-in 1)
;
; (propagate)

; 練習問題 3.30
(define (ripple-carry-adder alist blist slist c)
  (if (null? alist)
    'ok
    (let ((a (car alist)) (b (car blist)) (s (car slist)) (cout (make-wire)))
      (full-adder a b c s cout)
      (ripple-carry-adder (cdr alist) (cdr blist) (cdr slist) cout))))


; ; テスト
; (define alist (list (make-wire) (make-wire) (make-wire)))
; (define blist (list (make-wire) (make-wire) (make-wire)))
; (define slist (list (make-wire) (make-wire) (make-wire)))
; (define slist (list (make-wire) (make-wire) (make-wire)))
; (define c-in (make-wire))
;
; ; slistをすべてprobeする.
; (define (probe-all lst)
;   (define (probe-all-with-cnt ls n)
;     (if (null? ls)
;       'ok
;       (begin
;         (probe (string-append "out" (number->string n)) (car ls))
;         (probe-all-with-cnt (cdr ls) (+ n 1)))))
;   (probe-all-with-cnt lst 0))
; (probe-all slist)
;
; ; 伝播加算器生成
; (ripple-carry-adder alist blist slist c-in)
;
; ; 2 + 3 = 5
; (set-signal! (cadr alist) 1) ; 2
; (set-signal! (car blist) 1) (set-signal! (cadr blist) 1) ; 3
;
; (propagate)
; (get-signal (car slist))
; (get-signal (cadr slist))
; (get-signal (caddr slist))
