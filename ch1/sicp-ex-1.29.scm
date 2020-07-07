#lang sicp

; integral returns an approximation to the definite integral
; of function f from a to b using Simpson's rule with n steps,
; where n is assumed to be a positive even integer.
(define (integral f a b n)
  (define (simpson-approx h)
    (define (simpson-next x)
      (+ x (* 2 h)))
    (* (/ h 3)
       (+ (f a)
          (f b)
          (* 4 (sum f (+ a h) simpson-next b))
          (* 2 (sum f (+ a h h) simpson-next (- b h))))))
  (simpson-approx (/ (* 1.0 (- b a)) n))) ; multiplication by 1.0 makes h a float

; sum applies term to each input in the sequence
; a, (next a), (next (next a)), ..., not exceeding b,
; and returns the sum of the results.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(integral cube 0 1 100)  ; 0.2500000000000004
(integral cube 0 1 1000) ; 0.25000000000000083
