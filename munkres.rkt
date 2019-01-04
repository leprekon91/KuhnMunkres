#lang racket
(display "--- Kuhn-Munkres (Hungarian) Algorithm ---\n")

(define CostMatrix (list (list 1 2 1 4) (list 4 5 6 7) (list 7 8 9 0) (list 0 1 2 3)))

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

;find one in a list (for the mask matrix)
(define findOne (lambda(lst) 
  (cond 
    ( (null? lst) #f )
    ( (= (car lst) 1) #t )
    ( else ( findOne (cdr lst) ) )
  )))

;subtract minimum element from each member of the list
(define subMinFromList (lambda(lst)
    (begin
      (map (lambda(x) (- x (minInList lst))) lst )
    )
  )
)

; return a mask list of zeroes of the list that was received
(define maskList (lambda(lst) 
      (cond 
        ( (null? lst) (list))
        ( ( = ( car lst ) 0) ( cons 1 ( padList (cdr lst) ) ) )
        ( else ( cons 0 ( maskList (cdr lst) ) ) )
      )
  )
)
;return a list of zeroes the size of the ginen list
(define padList (lambda(lst) 
  ( if (null? lst)
    (list)
    (cons 0 (padList (cdr lst) ) ) )
  )
)
;---------------------------------------------------------------------------------------------------
; STEPS
;---------------------------------------------------------------------------------------------------

; Step 1: For Each Row Of The Cost Matrix: find minimum element and subtract
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
; Step 2: Find a zero (Z) in the resulting matrix.  If there is no starred zero in its row or column,
; star Z. Repeat for each element in the matrix.
(define stepTwo (lambda(lst)
    (if (null? lst) 
      (list)
      (cons
        ( maskList (car lst) ) 
        ( stepTwo (cdr lst) )
      )
    )
  )
)

(display "Showing Solution For Cost Matrix: ")
(display "\n\n")
(printMatrix CostMatrix)
(display "\n")
(display "-|-> Step 1 - processing\n\n")
(define stepOneMatrix (stepOne CostMatrix))
(printMatrix stepOneMatrix)
(display "-|-> Step 1 - complete\n")
(display "-|-> Step 2 - processing\n\n")
(define maskMatrix (stepTwo stepOneMatrix))
(printMatrix maskMatrix)
(display "-|-> Step 2 - complete\n")