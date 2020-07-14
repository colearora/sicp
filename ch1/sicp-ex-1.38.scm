#lang sicp

;===============================================================================
; Source
;===============================================================================

; Approximation for e using Euler's continued fraction expansion
; of e-2.
(define (e)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                (if (divides? 3 (+ i 1))
                    (/ (* 2.0 (+ i 1.0)) 3.0)
                    1.0))
                100)))

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

(define (divides? x y)
  (= (remainder y x) 0))

;===============================================================================
; Test
;===============================================================================

(e) ; 2.7182818284590455

