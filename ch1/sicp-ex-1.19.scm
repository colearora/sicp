#lang sicp

; fib: Returns the nth Fibonacci number in O(log n) time,
; for integer n >= 0.
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))

; Test
(define (test-fib i n)
  (cond ((< i n)
         (display (fib i))
         (newline)
         (test-fib (+ i 1) n))))
(test-fib 0 15)
  
