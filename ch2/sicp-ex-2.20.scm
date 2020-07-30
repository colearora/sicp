#lang sicp

;===============================================================================
; Source
;===============================================================================

; same-parity takes an integer x and a list of zero or more other
; integers args as inputs, and returns a list containing only those
; arguments that have the same parity (even/odd) as x, including x.
(define (same-parity x . args)
  (define (parity a) (remainder a 2)) ; even is 0, odd is 1
  (define (iter rmng res)
    (cond ((null? rmng) (reverse res))
          ((= (parity (car rmng)) (parity x))
           (iter (cdr rmng) (cons (car rmng) res)))
          (else
           (iter (cdr rmng) res))))
  (iter args (list x)))

(define (reverse items)
  (define (iter fwd rev)
    (if (null? fwd)
        rev
        (iter (cdr fwd) (cons (car fwd) rev))))
  (iter items nil))

;===============================================================================
; Test
;===============================================================================

(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; (2 4 6)