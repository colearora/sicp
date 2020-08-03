#lang sicp

;===============================================================================
; Source
;===============================================================================

(define x (list 1 3 (list 5 7) 9))
(define y (list (list 7)))
(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;===============================================================================
; Test
;===============================================================================

                         x      ; (1 3 (5 7) 9)    
(                    cdr x    ) ;   (3 (5 7) 9)
(               cdr (cdr x)   ) ;     ((5 7) 9)
(          car (cdr (cdr x))  ) ;      (5 7)
(     cdr (car (cdr (cdr x))) ) ;        (7)
(car (cdr (car (cdr (cdr x))))) ;         7
(newline)

          y   ; ((7))
(     car y ) ;  (7)
(car (car y)) ;   7
(newline)

                                                            z             ; (1 (2 (3 (4 (5 (6 7))))))
(                                                       cdr z           ) ;   ((2 (3 (4 (5 (6 7))))))
(                                                  car (cdr z)          ) ;    (2 (3 (4 (5 (6 7)))))
(                                             cdr (car (cdr z))         ) ;      ((3 (4 (5 (6 7)))))
(                                        car (cdr (car (cdr z)))        ) ;       (3 (4 (5 (6 7))))
(                                   cdr (car (cdr (car (cdr z))))       ) ;         ((4 (5 (6 7))))
(                              car (cdr (car (cdr (car (cdr z)))))      ) ;          (4 (5 (6 7)))
(                         cdr (car (cdr (car (cdr (car (cdr z))))))     ) ;            ((5 (6 7)))
(                    car (cdr (car (cdr (car (cdr (car (cdr z)))))))    ) ;             (5 (6 7))
(               cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))   ) ;               ((6 7))
(          car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))  ) ;                (6 7)
(     cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))) ) ;                  (7)
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))) ;                   7
(newline)
