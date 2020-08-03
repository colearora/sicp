#lang sicp

;===============================================================================
; Source
;===============================================================================

; WRONG: bad-square-list-1 conses each item encountered in items
; onto answer in the sub-procedure. The problem is that each item from
; items consed onto answer becomes the new "first item" of answer, and in
; particular the last item in items is the first item in the final answer.
(define (bad-square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; WRONG: bad-square-list-2 conses answer onto each new item encountered,
; which produces a structure that is not a proper list. Rather than the cars
; of the resulting pair sequence being the squared values, the cdrs of the
; list are values. Moreover the cdrs of the resulting pair sequence are still
; in the wrong order.
(define (bad-square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(define (square x) (* x x))

;===============================================================================
; Test
;===============================================================================

(bad-square-list-1 (list 1 2 3 4)) ; (16 9 4 1)
(bad-square-list-2 (list 1 2 3 4)) ; ((((() . 1) . 4) . 9) . 16)