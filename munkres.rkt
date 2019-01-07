#lang racket
(display "--- Kuhn-Munkres (Hungarian) Algorithm ---\n")

(define CostMatrix (list (list 1 2 1 4) (list 4 5 6 7) (list 7 8 9 1) (list 1 1 2 3)))

;---------------------------------------------------------------------------------------------------
; DEFINITIONS & HELPERS
;---------------------------------------------------------------------------------------------------

;get element i from list j in a matrix
(define getIJ (lambda(matrix i j) (list-ref (list-ref matrix j) i)))

;set element i in list j to val
(define setIJ (lambda(matrix i j val) (list-set (list-ref matrix j) i val)))

;pretty output for matrix in console
(define printMatrix (lambda(mat) 
  (if (null? mat)
    (display "\n")
    (begin
      (display (car mat)) 
      (display "\n")
      (printMatrix (cdr mat))
  )))
)

;subtract minimum element from each member of the list
(define subMinFromRow (lambda(lst)
    (begin
      (map (lambda(x) (- x (apply min lst))) lst )
    )
  )
)

; return a mask list of zeroes of the list that was received
(define maskList (lambda(lst) 
      (cond 
        ( (null? lst) (list))
        ( ( = ( car lst ) 0) ( cons 1 ( maskList (cdr lst) ) ) )
        ( else ( cons 0 ( maskList (cdr lst) ) ) )
      )
  )
)
;return a list of zeroes the size of the given list
(define padList (lambda(lst) 
  ( if (null? lst)
    (list)
    (cons 0 (padList (cdr lst) ) ) )
  )
)

;get a matrix and row index and nullify it
(define nullifyRow (lambda( matrix i ) 
  (list-set matrix i 
    (padList (list-ref matrix i))
  )
))

;get a matrix and column index and nullify it
(define nullifyCol (lambda( matrix i ) 
  (if (null? matrix) 
    (list)
    (cons 
      (list-set (car matrix) i 0)
      (nullifyCol (cdr matrix) i)
    )
  )
))
;simple transpose of a list of lists
(define transpose (lambda(matrix) (apply map list matrix)))
;---------------------------------------------------------------------------------------------------
; STEPS
;---------------------------------------------------------------------------------------------------

; Step 1: For Each Row Of The Cost Matrix: find minimum element and subtract
; it from each element in the row.
(define stepOne (lambda(lst)
    (if (null? lst) 
      (list)
      (cons
        ( subMinFromRow (car lst) ) 
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
(define minRowsMatrix (stepOne CostMatrix))
(define stepOneMatrix (transpose (stepOne (transpose minRowsMatrix))))
(printMatrix stepOneMatrix)
(display "-|-> Step 1 - complete\n\n")
(display "-|-> Step 2 - processing\n\n")
(define maskMatrix (stepTwo stepOneMatrix))
(printMatrix maskMatrix)
(display "-|-> Step 2 - complete\n\n")

(display "-|-> Step 3 - processing\n\n")
;TODO cover matrix zeroes with minimal lines
; Tick all unassigned rows
; Tick all (unticked) columns that have zeros in ticked rows
; Tick all (unticked) rows that have assigned zeros in ticked columns
; Go back to point 2 unless there are no more columns that need ticking
; Draw a line through every ticked column and every unticked row.
(display "-|-> Step 3 - complete\n\n")
