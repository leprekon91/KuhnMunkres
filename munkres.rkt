#lang racket
;---------------------------------------------------------------------------------------------------
; DEFINITIONS & HELPERS
;---------------------------------------------------------------------------------------------------

;;; Simple transpose of a list of lists
(define transpose (lambda(matrix) (apply map list matrix)))

(define (makeIndexList n)
  (if (= n 0)
     (list 0)                       ; base case. Just return (0)
     (cons n (makeIndexList (- n 1)))))  ; recursive case. Add n to the head of (n-1 n-2 ... 0)

;return the first index at which a given element [val] can be found in the list [lst]
(define indexOf
  (lambda (e lst)
    (if (null? lst)
      -1
      (if (eq? (car lst) e)
        0
        (if (= (indexOf e (cdr lst)) -1) 
          -1
          (+ 1 (indexOf e (cdr lst)))
        )
      )
    )
  )
)

;;; Count Members of a list
(define (howMany lst)
  (cond ((null? lst) 0)
        ((not (pair? lst)) 1)
        (else (+ (howMany (car lst))
                 (howMany (cdr lst))))))

;;; Get last element of a list
(define (lastElem list) (car (reverse list)))

;;; Pretty Output for matrix in console
(define printMatrix (lambda(mat) 
    (if (null? mat)
      (display "\n")
      (begin
        (display (car mat))
        (display "\n")
        (printMatrix (cdr mat))
      )
    )
  )
)


;;; Does list [lst] has element [x]?
(define hasElement ( lambda(lst x) ( not ( null? ( filter ( lambda(e) (= e x)) lst ) ) ) ) )

;;; Does row [i] of matrix [mat] has element [x]?
(define rowHasElement (lambda(matrix i val)
    (if (= i 0)
      ( hasElement (car matrix) val )
      ( rowHasElement (cdr matrix) (- i 1) val )
    )
  )
)

;;; Does col [i] of matrix [mat] has element [x]?
(define colHasElement (lambda( matrix i val) (rowHasElement (transpose matrix) i val)))

;---------------------------------------------------------------------------------------------------
(display "--- Kuhn-Munkres (Hungarian) Algorithm ---\n")

(define CostMatrix (list 
    (list 4 3 2 1)
    (list 7 6 5 4)
    (list 1 9 8 7)
    (list 3 2 1 1)
  )
)

(define N (howMany (car CostMatrix)))

; For Each Row Of a matrix: find minimum element in a row and subtract
; it from each element in that row.
(define normalize (lambda(lst)
    (if (null? lst) 
      (list)
      (cons
        (map (lambda(x) (- x (apply min (car lst)))) (car lst) )
        (normalize (cdr lst) )
      )
    )
  )
)

(printMatrix CostMatrix)
(define normalizedMatrix (transpose (normalize (transpose (normalize CostMatrix)))))
(printMatrix normalizedMatrix)

(define list_zero_indexes (lambda(lst i)
    (if (null? lst) 
      (list)
      (if (and (= (car lst) 0))
        (cons (list (quotient i N) (modulo i N)) (list_zero_indexes (cdr lst) (+ i 1)))
        (list_zero_indexes (cdr lst) (+ i 1))
      )
    )
  )
)

(define zero_indices (list_zero_indexes (apply append normalizedMatrix) 0))
;;; Remove all indices that are row [i] or col [j]
(define filter_indices (lambda(lst i j) 
    (filter 
      (lambda(xyIndex)
        (not 
          (or
            (= i (car xyIndex))
            (= j (lastElem xyIndex))
          )
        )
      ) 
      lst
    )
  )
)


(define starZeroes (lambda(lst)
    (if (null? lst) 
      (list)
      (cons 
        (car lst)
        (starZeroes
          (filter
            (lambda(xyIndex)
              (not 
                (or
                  (= (car (car lst)) (car xyIndex))
                  (= (lastElem (car lst)) (lastElem xyIndex))
                )
              )
            ) 
            (cdr lst)
          )
        )
      )
    )
  )
)

(define assignment (starZeroes zero_indices))
(display assignment)
(display "\n")

;;; if the starred zeroes size is N, than this is the assignment
;;; else, create additional zeroes:

;;; find minimal cover:

;;;;1 Tick all unassigned rows
;return a list of ticked unassigned rows
(define tickUnassignedRows (lambda(ass) (filter (lambda(x) (not (hasElement (car( transpose ass)) x))) (makeIndexList (- N 1)))))
(display (tickUnassignedRows assignment))
;;;;2 Tick all (unticked) columns that have zeros in ticked rows


(display (indexOf 10 (car normalizedMatrix)))
;;;;3 Tick all (unticked) rows that have assigned zeros in ticked columns
;;;;4 Go back to point 2 unless there are no more columns that need ticking
;;;;5 Draw a line through every ticked column and every unticked row.
;;; find minimum uncovered element (index is not in starred zeroes list)
;;; subtract from all uncovered elements the minimum found
;;; add minimum to all elements that are covered twice
