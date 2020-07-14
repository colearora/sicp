#lang sicp

;===============================================================================
; Source
;===============================================================================

; tan-cf returns an approximation to tan(x) using
; Lambert's continued fraction expansion.
(define (tan-cf x k)
  (cont-frac (lambda (i) (fast-expt x i))
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

; fast-expt returns the nth power of b in O(log n) time.
(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (square x)
  (* x x))
 
;===============================================================================
; Test
;===============================================================================

(tan-cf 0.00 100) ; 0.0
(tan-cf 0.25 100) ; 0.25532481426449716
(tan-cf 0.50 100) ; 0.5458723128641024
(tan-cf 0.75 100) ; 0.9293417567017108
(tan-cf 1.00 100) ; 1.557407724654902

; Note: tan-cf becomes increasingly inaccurate as input
; x becomes increasingly larger than 1.

