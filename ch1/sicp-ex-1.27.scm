#lang sicp

; carmichael? returns true if n is a Carmichael number,
; or a non-prime integer that fools the Fermat test.
(define (carmichael? n)
  ; fermat-iter applies the Fermat test to n using test
  ; values in [a..n), returning true if all tests pass.
  (define (fermat-iter a)
    (cond ((= a n) true) ; all tests passed
          ((= (expmod a n n) a) (fermat-iter (+ a 1)))
          (else false)))        
  (if (fermat-iter 1)
      (not (prime? n))
      false))

; expmod returns base^exp mod m in O(log exp) time.
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

; prime? returns true if n >= 2 is prime, false otherwise.
; Runtime is O(sqrt(n)).
(define (prime? n)
  (= n (smallest-divisor n)))

; smallest-divisor returns the smallest integer
; in [2..n] that divides integer n >= 2.
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next x)
    (if (= x 2) 3 (+ x 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))

; Known Carmichael numbers include:
; 561, 1105, 1729, 2465, 2821, 6601.
(carmichael? 13)    ; false
(carmichael? 199)   ; false
(carmichael? 1999)  ; false
(carmichael? 19999) ; false
(carmichael? 561)   ; true
(carmichael? 1105)  ; true
(carmichael? 1729)  ; true
(carmichael? 2465)  ; true
(carmichael? 2821)  ; true
(carmichael? 6601)  ; true