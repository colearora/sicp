#lang sicp

;===============================================================================
; Source - infix parsing
;===============================================================================

; Dijkstra's shunting-yard algorithm for parsing infix arithmetic expressions.
; Returns an equivalent expression in prefix (i.e., Scheme) notation.
; See: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
(define (parse infix)
  (define (iter infix op-stack prefix)
    (cond ((and (null? infix) (null? op-stack))
           (if (null? prefix) '() (car prefix)))
          ((null? infix)
           (iter infix (cdr op-stack) (add-op (car op-stack) prefix)))
          ((pair? (car infix))
           (iter (cdr infix) op-stack (cons (parse (car infix)) prefix)))
          ((op? (car infix))
           (if (or (null? op-stack)
                   (> (precedence (car infix)) (precedence (car op-stack)))
                   (and (= (precedence (car infix)) (precedence (car op-stack)))
                        (eq? (associativity (car infix)) 'left)))
                (iter (cdr infix) (cons (car infix) op-stack) prefix)
                (iter infix (cdr op-stack) (add-op (car op-stack) prefix))))
          (else
           (iter (cdr infix) op-stack (cons (car infix) prefix)))))
  (iter (reverse infix) '() '()))

(define operators '(+ - * / **))
(define (op? s)
  (any? operators (lambda (op) (eq? op s))))

(define (add-op op prefix)
  (let ((arg1 (car prefix))
        (arg2 (cadr prefix)))
    (cons (list op arg1 arg2) (cddr prefix))))

(define (precedence op)
  (cond ((eq? op '**) 4)
        ((eq? op '* ) 3)
        ((eq? op '/ ) 3)
        ((eq? op '+ ) 2)
        ((eq? op '- ) 2)
        (else (error "precedence: undefined: " op))))

(define (associativity op)
  (cond ((eq? op '**) 'right)
        ((eq? op '* ) 'left )
        ((eq? op '/ ) 'left )
        ((eq? op '+ ) 'left )
        ((eq? op '- ) 'left )
        (else (error "associativity: undefined: " op)))) 

(define (reverse items)
  (define (iter fwd rev)
    (if (null? fwd)
        rev
        (iter (cdr fwd) (cons (car fwd) rev))))
  (iter items '()))

(define (any? items predicate)
  (cond ((null? items)
         false)
        ((predicate (car items))
         true)
        (else
         (any? (cdr items) predicate))))

;===============================================================================
; Source - symbolic differentiation
; (Same as in exercise 2.57)
;===============================================================================

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product e
                         (make-product (make-exponentiation b (make-sum e -1))
                                       (deriv b var)))))
        (else
         (error "deriv: unknown expression type: " exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s) ; sum consists of two terms; return second
      (cons '+ (cddr s)))) ; sum consists of more than two terms

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p) ; product consists of two terms; return second
      (cons '* (cddr p)))) ; product consists of more than two terms

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        (else (list '** u n))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;===============================================================================
; Test - infix parsing
;===============================================================================

'()
(parse '()) ; ()
(display "\n")

'(a)
(parse '(a)) ; a
(display "\n")

'(a + b)
(parse '(a + b)) ; (+ a b)
(display "\n")

'((a + b))
(parse '((a + b))) ; (+ a b)
(display "\n")

'(a + b - c)
(parse '(a + b - c)) ; (- (+ a b) c)
(display "\n")

'(a + (b - c))
(parse '(a + (b - c))) ; (+ a (- b c))
(display "\n")

'(a * b - c)
(parse '(a * b - c)) ; (- (* a b) c)
(display "\n")

'((a * b) - c)
(parse '((a * b) - c)) ; (- (* a b) c)
(display "\n")

'(a * (b - c))
(parse '(a * (b - c))) ; (* a (- b c))
(display "\n")

'((a * b - c))
(parse '((a * b - c))) ; (- (* a b) c)
(display "\n")

'(a + b * c - d)
(parse '(a + b * c - d)) ; (- (+ a (* b c)) d)
(display "\n")

'(a + (b * c) - d)
(parse '(a + (b * c) - d)) ; (- (+ a (* b c)) d)
(display "\n")

'(a + b * (c - d))
(parse '(a + b * (c - d))) ; (+ a (* b (- c d)))
(display "\n")

'((a + b) * c - d)
(parse '((a + b) * c - d)) ; (- (* (+ a b) c) d)
(display "\n")

'((a + b) * (c - d))
(parse '((a + b) * (c - d))) ; (* (+ a b) (- c d))
(display "\n")

'(2 ** 3 ** 3)
(parse '(2 ** 3 ** 3)) ; (** 2 (** 3 3))
(display "\n")

'(3 + 4 * 2 / (1 - 5) ** 2 ** 3)
(parse '(3 + 4 * 2 / (1 - 5) ** 2 ** 3)) ; (+ 3 (/ (* 4 2) (** (- 1 5) (** 2 3))))
(display "\n")

'(x + 3 * (x + y + 2))
(parse '(x + 3 * (x + y + 2)))
(display "\n")

;===============================================================================
; Test - symbolic differentiation
;===============================================================================

(deriv (parse '(x + 3 * (x + y + 2))) 'x) ; 4

