#lang slideshow

(define (filled-square width color) (filled-rectangle width width #:color color))

(define (render-bit bit) (
                          cond
                          [(= bit 1) (filled-square 15 "black")]
                          [(= bit 0) (filled-square 15 "white")]
                          [else (error "wrong bit value")]
                         )
)

(define (render-bits bits)
  (cond
    [(empty? bits) (blank)]
    [else (hc-append 1 (render-bit (first bits)) (render-bits (rest bits)))]
  )
)

(define (count-ones seq)
  (cond
    [(empty? seq) 0]
    [else (+ (first seq) (count-ones(rest seq)))]
  )
)

(define (count-ones-improved seq)
  (define (helper seq current)
    (cond
      [(empty? seq) current]
      [else (helper (rest seq) (+ current (first seq)))]
    )
  )
  (helper seq 0)
)


(define (trailing-zeros seq)
  (define (helper seq current)
    (cond
      [(empty? seq) current]
      [(= (first seq) 1) (helper (rest seq) 0)]
      [else (helper (rest seq) (+ current 1))]
    )
  )
  (helper seq 0)
)

