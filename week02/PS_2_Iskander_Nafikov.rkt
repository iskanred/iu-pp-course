#lang slideshow

; ========== Problem set # 2 =================
; ============================================
; ======== Iskander Nafikov BS20-SD01 ========


; ==== Problem 1 ====

; a)
(define (binary-to-decimal binary)
  (define (helper seq result pow)
    (cond
      [(empty? seq) result]
      [else (helper (rest seq) (+ result (* (first seq) pow)) (* pow 2))]
    )
  )
  (helper (reverse binary) 0 1)
)

; b)
; (cut-leading-zeros '(0 0 0 0 ... 0)) -> 0
(define (cut-leading-zeros seq)
  (cond
    [(empty? seq) '(0)]
    [(= (first seq) 1) seq]
    [else (= (first seq) 0) (cut-leading-zeros (rest seq))]
  )
)

; (count-zeros '(0 0 0 0 ... 0)) -> 1
(define (count-zeros seq)
  (define (helper seq result)
    (cond
      [(empty? seq) result]
      [(= (first seq) 0) (helper (rest seq) (+ result 1))]
      [else (helper (rest seq) result)]
    )
  )
  (helper (cut-leading-zeros seq) 0)
)

; c)
(define (opposite bit)
  (abs (- bit 1))
)

(define (encode-with-lengths seq)
  (define (helper seq bit accum result)
    (cond
      [(empty? seq) (append result (list accum))]
      [(= (first seq) bit) (helper (rest seq) bit (+ accum 1) result)]
      [else (helper (rest seq) (opposite bit) 1 (append result (list accum)))]
    )
  )
  (define cuted-seq (cut-leading-zeros seq))
  (helper cuted-seq (first cuted-seq) 0 '())
)

; d)
(define (binary-odd? binary)
  (cond
    [(empty? binary) #f]
    [else (= (last binary) 1)]
  )
)

; e)
(define (decrement binary)
  (define (helper seq result copy?)
    (cond
      [(empty? seq) result]
      ; [seq is '(0)]
      [(and (= (length seq) 1) (= (first seq) 0)) seq]
      [copy? (helper (rest seq) (append result (list (first seq))) #t)]
      [(= (first seq) 1) (helper (rest seq) (append result '(0)) #t)]
      [else (helper (rest seq) (append result '(1)) #f)]
    )
  )
  (cut-leading-zeros (reverse (helper (reverse binary) '() #f)))
)


; ==== Problem 2 ====

; a)
(define (alternating-sum seq)
  (cond
    [(empty? seq) 0]
    [(= (length seq) 1) (first seq)]
    [else (+ (alternating-sum (rest (rest seq))) (- (first seq) (second seq)))]
  )
)

; b)
; (alternating-sum '(1 2 3 4 5)) ->
; (+ (alternating-sum '(3 4 5)) (- 1 2)) ->
; (+ (+ (alternating-sum '(5)) (- 3 4)) (- 1 2)) ->
; (+ (+ 5 (- 3 4)) (- 1 2)) ->
; (+ 4 (- 1 2)) ->
; 3


; c)
(define (alternating-sum-improved seq)
  (define (helper seq sum is-plus?)
    (cond
      [(empty? seq) sum]
      [is-plus? (helper (rest seq) (+ sum (first seq)) #f)]
      [else (helper (rest seq) (- sum (first seq)) #t)]
    )
  )
  (helper seq 0 #t)
)

; (alternating-sum-improved '(1 2 3 4 5)) ->
; (helper '(1 2 3 4 5) 0 #t) ->
; (helper '(2 3 4 5) 1 #f) ->
; (helper '(3 4 5) -1 #t) ->
; (helper '(4 5) 2 #f) ->
; (helper '(5) -2 #t) ->
; (helper '() 3 #f) ->
; 3

; We see that optimization helps us not to remember
;  a long expression with inner recursion call because
;  we have accumulator variable which is passed to each
;  recustion call.
; Also, it reduces time because with tail-recursion
;  there is no need on each step to execute functions which
;  present in "return" expression
; However, we can see that number of recursive calls is less in
;  explicit recursion implementation


; ==== Problem 3 ====

; (f 3) ->
; (* (f (dec (dec 3))) (f (dec 3))) ->
; (* (f (dec (- 3 1))) (f (- 3 1))) ->
; (* (f (dec 2)) (f 2)) ->
; (* (f (- 2 1)) (f 2)) ->
; (* (f 1) (f 2)) ->
; (* (- 10 1) (- 10 2)) ->
; (* 9 8) ->
; 72





