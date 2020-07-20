#lang sicp

;===============================================================================
; Source
;===============================================================================

; cubic returns a procedure of one argument x that performs the
; computation x^3 + ax^2 + bx + c.
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

; newtons-method returns a zero of the single-argument procedure g,
; which is likely to converge if guess is sufficiently close to the zero.
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; newton-transform takes a procedure of one argument g
; for which zeros are sought and returns a procedure one argument
; whose fixed points correspond to the zeros of g.
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

; deriv takes as input a procedure of one argument g
; and returns a procedure that approximates the derivative
; of g. 
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

; fixed-point performs a search for a fixed point of function f
; by repeatedly applying f to first-guess until the difference
; between input and output is less than some tolerance.
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define tolerance 0.00001)

(define (cube x) (* x x x))
(define (square x) (* x x))
 
;===============================================================================
; Test
;===============================================================================

; Has one root:
(newtons-method (cubic 2 1 3) -4) ; -2.1745594102885435

; Has three roots:
(newtons-method (cubic 2 -7 3) -5) ; -3.9593414411740513
(newtons-method (cubic 2 -7 3)  0) ;  0.5301677113370412
(newtons-method (cubic 2 -7 3)  2) ;  1.429173729866978





