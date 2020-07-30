#lang sicp

;===============================================================================
; Source
;===============================================================================

; reverse returns a list containing the elements of the given list
; items in reverse order.
(define (reverse items)
  (define (iter fwd rev)
    (if (null? fwd)
        rev
        (iter (cdr fwd) (cons (car fwd) rev))))
  (iter items nil))

;===============================================================================
; Test
;===============================================================================

(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)
(reverse (list 3)) ; (3)
(reverse nil) ; ()
