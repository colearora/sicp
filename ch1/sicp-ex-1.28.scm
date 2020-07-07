#lang sicp

; fast-prime? returns true if n passes times applications of
; the Miller-Rabin test, false otherwise.
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

; miller-rabin-test applies the Miller-Rabin
; primality test to n, returning true if n passes
; and false otherwise.
(define (miller-rabin-test n)
  (define (try-it a)
    (= (checked-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

; checked-expmod returns b^e mod m unless in the course of
; computation a non-trivial square root of 1 mod m is
; discovered, in which case 0 is returned.
(define (checked-expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder
          (checked-square (checked-expmod b (/ e 2) m) m)
          m))
        (else
         (remainder
          (* b (checked-expmod b (- e 1) m))
          m))))

; checked-square returns the square of x unless x is a
; non-trivial square root of 1 mod n -- that is, a number
; equal to neither 1 nor n-1 whose square when divided by
; n gives a remainder of 1 -- in which case 0 is returned.
(define (checked-square x n)
  (if (and (> x 1)
           (< x (- n 1))
           (= (remainder (square x) n) 1))
      0
      (square x)))

(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))

; Note: the applications of the Miller-Rabin primality test
; below use a fast-prime? implementation that arbitrarily applies the test
; 10 times for a given number under test, reducing the probability
; of error to less than (1/2)^10. That is, the probability
; of observing 10 consecutive test passes given the number is not prime
; is less than (1/2)^10.

; Known non-primes:
(fast-prime? 4 10)     ; false
(fast-prime? 6 10)     ; false
(fast-prime? 20 10)    ; false
(fast-prime? 60 10)    ; false
(fast-prime? 121 10)   ; false
(fast-prime? 19999 10) ; false
(newline)

; Carmichael numbers
; (non-primes that fool the Fermat test):
(fast-prime? 561 10)  ; false
(fast-prime? 1105 10) ; false
(fast-prime? 1729 10) ; false
(fast-prime? 2465 10) ; false
(fast-prime? 2821 10) ; false
(fast-prime? 6601 10) ; false
(newline)

; Known primes:
(fast-prime? 2 10)    ; true
(fast-prime? 3 10)    ; true
(fast-prime? 5 10)    ; true
(fast-prime? 7 10)    ; true
(fast-prime? 11 10)   ; true
(fast-prime? 13 10)   ; true
(fast-prime? 17 10)   ; true
(fast-prime? 19 10)   ; true
(fast-prime? 23 10)   ; true
(fast-prime? 199 10)  ; true
(fast-prime? 1999 10) ; true


