#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define fold-right accumulate)

;===============================================================================
; Test
;===============================================================================

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)

; Note: properties that op should satisfy to guarantee that
; fold-right and fold-left will produce the same values for any
; sequence include:
;     1. op should be commutative: for any a and b, it should be the
;        case that (op a b) = (op b a).
;     2. op should be associative: for any a, b, and c, it should be the
;        case that (op a (op b c)) = (op (op a b) c).
