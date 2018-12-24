#lang racket
(display "--- Kuhn-Munkres (Hungarian) Algorithm ---\n")

(define CostMatrix (list (list 1 2 3 4) (list 4 5 6 7) (list 7 8 9 0) (list 0 1 2 3)))

;---------------------------------------------------------------------------------------------------
; DEFINITIONS & HELPERS
;---------------------------------------------------------------------------------------------------

;print matrix beautifully in console
(define printMatrix (lambda(mat) 
  (if (null? mat)
    (display "\n")
    (begin
      (display (car mat)) 
      (display "\n")
      (printMatrix (cdr mat))
  )))
)

;find minimum element in list
(define minInList (lambda(lst) 
    (apply min lst)
  )
)

;subtract minimum element from each member of the list
(define subMinFromList (lambda(lst)
    (begin
      (map (lambda(x) (- x (minInList lst))) lst )
    )
  )
)

;---------------------------------------------------------------------------------------------------
; STEPS
;---------------------------------------------------------------------------------------------------

; For Each Row Of The Cost Matrix: find minimum element and subtract
; it from each element in the row.
(define stepOne (lambda(lst)
    (if (null? lst) 
      (list)
      (cons
        ( subMinFromList (car lst) ) 
        ( stepOne (cdr lst) )
      )
    )
  )
)



(display "Showing Solution For Cost Matrix: ")
(display "\n\n")
(printMatrix CostMatrix)
(display "\n")
(display "-|-> Step 1 - processing\n\n")
(printMatrix ( stepOne CostMatrix ))
(display "-|-> Step 1 - complete\n")