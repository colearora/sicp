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
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; fast-prime?: Returns true if n passes times applications of
; the Fermat test, false otherwise.
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; fermat-test: Returns true if, given a random number a in [1..n-1],
; it is the case that a^n mod n = a, and false otherwise. This is the Fermat test
; for primality, running in O(log n) time. A false result means that
; n is definitely not prime; a true result means that n is probably prime.
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; expmod: Returns base^exp mod m in O(log exp) time.
; Relies on the fact that given any integers x, y, and m,
; xy mod m = (x mod m)(y mod m) mod m.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

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

; Observation: The time required by fast-prime? is about T(n) = c lg n for 
; some small constant c. Experimentation on this machine with primes around 1000
; suggests c is about 3, s.t. T(n) = 3 lg n. Thus multiplying the input size by
; 1000 should increase the runtime to T(1000n) = 3 lg 1000 + 3 lg n, which is about
; 30 + T(n). This is roughly what is seen when comparing T(1000) with T(1000000).
