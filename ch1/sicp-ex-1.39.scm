#lang sicp

;===============================================================================
; Source
;===============================================================================

; tan-cf returns an approximation to tan(x) using
; Lambert's continued fraction expansion.
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (square x)))
             (lambda (i) (+ (* 2 (- i 1)) 1))
             -
             k))

; cont-frac returns the k-term finite continued fraction.
; (Iterative implementation.)
(define (cont-frac n d combiner k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1)
              (/ (n i)
                 (combiner (d i) res)))))
  (iter k 0.0))

(define (square x)
  (* x x))
 
;===============================================================================
; Test
;===============================================================================

(tan-cf 0.00 100) ; 0.0
(tan-cf 0.25 100) ; 0.25534192122103627
(tan-cf 0.50 100) ; 0.5463024898437905
(tan-cf 0.75 100) ; 0.9315964599440725
(tan-cf 1.00 100) ; 1.557407724654902
(tan-cf 1.25 100) ; 3.009569673862831
(tan-cf 1.50 100) ; 14.101419947171719
(tan-cf 1.55 100) ; 48.078482479219396
(tan-cf 1.57 100) ; 1255.7655915008268


