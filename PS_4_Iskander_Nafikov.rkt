#lang racket

; ====================================================================
; =========================== Problem Set 4 ===========================
; =================== Iskander Nafikov B20-SD01 ========================
; ============================ 24/09/22 ===============================
; ====================================================================


; =================== Utility Functions ===================

(define (not-empty? seq) (not (empty? seq)))

; note that (a, b) = (b, a)
(define (same-unordered-pair? p1 p2)
  (or
   (and (eq? (car p1) (cdr p2)) (eq? (cdr p1) (cdr p2)))
   (and (eq? (car p1) (cdr p2)) (eq? (cdr p1) (car p2)))))

; builds list of all possible pairs of elements of seq
(define (ordered-pairs seq)
  ; all pairs from the seq with the same first item of seq
  (define (pairs-same-first item)
    (filter ; filter not-empty lists
     not-empty?
     (map (lambda (a b)
            (if (eq? a b) '() (cons a b)))
          (replicate (length seq) item)
          seq)))
  
   ; generate pairs with the same first item for all items of seq and append them
   (foldl append '() (map pairs-same-first seq)))

; informed from https://stackoverflow.com/a/9370147/11825114
(define (my-remove-duplicates seq [same? eq?])
  (foldl
   (lambda (a b)
           (cons a (filter (lambda (c) (not (same? a c))) b)))
   '()
   seq))

; finds max element from seq
(define (my-max seq [greater? >])
  (foldl
   (lambda (item cur-max) (if (greater? item cur-max) item cur-max))
   (first seq)
   seq))

; finds min element from seq
(define (my-min seq [less? <])
  (foldl
   (lambda (item cur-min) (if (less? item cur-min) item cur-min))
   (first seq)
   seq))

; to determine which pair's operation is greater than another
(define (pair-binary-op-greater? p1 p2 op)
  (> (op (car p1) (cdr p1))
     (op (car p2) (cdr p2))))



; =================== Exercise 1 ===================

; a)
(define (replicate n val)
  (if (<= n 0)
      '()
      (cons val (replicate (- n 1) val))))


; b)
; solution is pure recustion (not tail one)
(define (split seq prefix-size)
  (cond
    [(<= prefix-size 0) (list '() seq)]
    [(empty? seq) (list '() '())]
    [else
     (cons
      (cons (first seq) (first (split (rest seq) (- prefix-size 1))))
      (rest (split (rest seq) (- prefix-size 1))))]))


; c)
; current-chunk is a sort of accumulator, so that the resursion is tail one
;  but without helper and with presence of optional parameter
(define (chunks seq size [current-chunk '()])
  (cond
    [(<= size 0) (error "size must be positive")]
    [(empty? seq) (list current-chunk)]
    [(= (length current-chunk) size)
     (cons current-chunk (chunks (rest seq) size (list (first seq))))]
    [else
     (chunks (rest seq) size (append current-chunk (list (first seq))))]))

; d)
; tail recursion
(define (windows seq size [current-window '()])
  (cond
    [(<= size  0) (error "size cannot be less than 0")]
    [(empty? seq) (list current-window)]
    [(= (length current-window) size)
     (cons
      current-window
      (windows (rest seq) size (append (rest current-window) (list (first seq)))))]
    [else
     (windows (rest seq) size (append current-window (list (first seq))))]))


; =================== Exercise 2 ===================

; a)
(define (pairs seq)
  ; remove same unordered pairs
  (my-remove-duplicates (ordered-pairs seq) same-unordered-pair?))

; b)
(define (splits seq)
  (map
   (lambda (prefix-size) (split seq prefix-size))
   (sequence->list (in-inclusive-range 0 (length seq)))))

; c)
(define (max-product seq) (max-binary-op * seq))

; d)
(define (max-binary-op op seq)
  ; uses function (ordered-pairs) from "utility functions" section
  (my-max (ordered-pairs seq) (lambda (p1 p2)
                        (pair-binary-op-greater? p1 p2 op))))

; e)
; NOT IMPLEMENTED
(define (combinations)
  empty)


; =================== Exercise 3 ===================

; a)
(define (max seq) (my-max seq))

; b)
(define (max-less-than-bound seq max_bound [greater? >] [less? <])
  (foldl
   (lambda (item cur-max)
     (cond
       [(and (greater? item cur-max) (less? item max_bound)) item]
       [else cur-max]))
   (my-min seq)
   seq))

; second-max
(define (second-max seq)
  (max-less-than-bound seq (my-max seq)))

; c)
(define (top-3 seq)
  (list (my-max seq)
        (second-max seq)
        (max-less-than-bound seq (second-max seq))))

; d)
; NOT IMPLEMENTED

; e)
(define (sum seq)
  (foldl + 0 seq))

; cumulative-sums
(define (cumulative-sums seq)
  (foldl
   (lambda (item result)
     (append result (list (sum (first item)))))
   '()
   (splits seq)))




