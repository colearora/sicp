#lang sicp

;===============================================================================
; Source
;===============================================================================

; cont-frac returns the k-term finite continued fraction.
; (Iterative implementation.)
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) res)))))
  (iter k 0))

;===============================================================================
; Test
;===============================================================================

; Approximation to 1/phi ~= 0.6180344478216819.
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100) ; 0.6180339887498948

