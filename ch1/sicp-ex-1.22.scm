#lang sicp

; search-for-primes: Checks every odd integer in [lo..hi]
; for primality, printing the results.
(define (search-for-primes lo hi)
  (define (search-iter candidate)
    (cond ((<= candidate hi)
           (timed-prime-test candidate)
           (search-iter (+ candidate 2)))))
  (if (even? lo)
      (search-iter (+ lo 1))
      (search-iter lo)))

; timed-prime-test: Runs a primality test on n.
; Prints n and if n is prime, also prints the (wall clock)
; time for the test in units of the primitive procedure runtime.
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; prime?: Returns true if n >= 2 is prime, false otherwise.
; Runtime is O(sqrt(n)).
(define (prime? n)
  (= n (smallest-divisor n)))

; smallest-divisor: Returns the smallest integer
; in [2..n] that divides integer n >= 2.
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))

(search-for-primes 1000 1020) ; three smallest primes > 1000
                              ; are 1009, 1013, 1019

(search-for-primes 10000 10038) ; three smallest primes > 10000
                                ; are 10007, 10009, 10037

(search-for-primes 1000000 1000038) ; three smallest primes > 1000000
                                    ; are 1000003, 1000033, 1000037

; Observation: The time required to test the primality of 1000037
; is about 10x the amount of time required to test the primality of 10037.
; I.e., when the input size is multiplied by 100, the time required is multiplied by 10.
; This is consistent with the O(sqrt(n)) complexity analysis of the prime? procedure.

