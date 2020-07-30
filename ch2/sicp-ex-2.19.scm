#lang sicp

;===============================================================================
; Source
;===============================================================================

; cc takes as inputs an amount for which to make change and
; a list of the values of the coins to use (in the same denomination
; as amount), and returns the total number of ways to make change
; using those coins. The order of coins in coin-values does not matter.
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))

;===============================================================================
; Test
;===============================================================================

(define us-coins (list 50 25 10 5 1))
(define us-coins-scrambled (list 5 10 1 50 25))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins) ; 292 (ways to make change for 1 dollar)
(cc 100 us-coins-scrambled) ; 292
(cc 100 uk-coins) ; 104561 (ways to make change for 1 pound)
