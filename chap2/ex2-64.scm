(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
; partial-treeの戻り値は,
; (構築した木,木に含まなかった要素)のペア

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
; a.
;   nを中央の値を残して2つに分ける場合の左側のサイズを決定する.
;   左側をpartial-treeする
;   右のサイズを決定する.
;   残った要素の先頭は中央値として残し,
;   右側をpartial-treeする
;   できた左右の木と中央値で木を作り,残りの要素と友に返す
;;   個人的には,木のアルゴリズムよりも,中央値を残して左右のサイズを決定する方法を
;;   適切に選ぶことがポイントだった.

; b.
;   各nに対するconsは一度だけ行うので O(n)


