#lang racket
(display "--- Kuhn-Munkres (Hungarian) Algorithm ---\n")

(define CostMatrix (list (list 1 2 3 4) (list 4 5 6 7) (list 7 8 9 0) (list 0 1 2 3)))

;---------------------------------------------------------------------------------------------------
; DEFINITIONS
;---------------------------------------------------------------------------------------------------

;find minimum element in list
(define minInList (lambda(lst) 
    (apply min lst)
  )
)

;subtract minimum element from each element in a list
(define subMinFromList (lambda(lst)
    (map (lambda(x) (- x (minInList lst))) lst )
  )
)

;---------------------------------------------------------------------------------------------------
; STEPS
;---------------------------------------------------------------------------------------------------

; For Each Row Of The Cost Matrix: find minimum element and subtract
; it from each element in the row.
(define stepOne (lambda(lst)
    (if (null? lst) 
      (lst)
      (cons
        ( subMinFromList (car lst) ) 
        ( stepOne (cdr lst) )
      )
    )
  )
)

(display "Showing Solution For Cost Matrix: ")
(display "\n")
(display CostMatrix)
(display "\n\n")
(display "-|-> Step 1 - processing\n")
(display ( stepOne CostMatrix ))
(display "-|-> Step 1 - complete\n")