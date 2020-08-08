#lang sicp

;===============================================================================
; Source
;===============================================================================

; square-tree takes a tree with numeric leaves as argument and returns
; a tree having the same shape but with each leaf squared.
(define (square-tree tree)
  (tree-map square tree))

; tree-map takes a procedure and tree as arguments, and returns a new tree of
; the same shape where each leaf on the new tree is the result of application
; of the procedure to the corresponing leaf on the old tree.
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

; map takes a procedure and list as arguments, and returns a new list of
; the same length where each node on the new list is the result of application
; of the procedure to the corresponding node on the old list.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

;===============================================================================
; Test
;===============================================================================

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ; (1 (4 (9 16) 25) (36 49))
