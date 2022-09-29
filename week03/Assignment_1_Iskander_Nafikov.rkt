#lang racket

; ====================================================================
; =========================== Assignment 1 ============================
; =================== Iskander Nafikov B20-SD01 ========================
; ============================ 21/09/22 ===============================
; ====================================================================



; ======================================================================
; ======================= Global Helper Functions =======================
; ======================================================================

; ======= Validation =======

; Check whether a given expression is valid logarithm operation.
; Valid in this case means that its operand is positive if it is number
(define (valid-log-op? expr)
  (if (and (log? expr) (number? (second expr)))
      (> (second expr) 0)
      (log? expr)))

; Check whether a given expression is valid supported mathematical unary/binary operation.
; Only top-level operation is validated, operands are not validated
;  excepting for logarithm where operand must be greater than 0 if it is number
(define (valid-unary-binary-op? expr)
  (or
   (sum? expr)
   (product? expr)
   (expt? expr)
   (sin? expr)
   (cos? expr)
   (tan? expr)
   (valid-log-op? expr)))

; Check whether a given expression is valid supported mathematical polyvariadic operation.
; Note that any unary/binary operation is a valid polyvariadic operation.
; Only top-level operation is validated, operands are not validated.
(define (valid-polyvariadic-op? expr)
  (or
   (valid-unary-binary-op? expr) ; any unary/binary operation is a valid polyvariadic operation
   (polyvariadic-sum? expr)
   (polyvariadic-product? expr)))


; ======= Operation check =======

; Check whether a given expression is an unary operation (e.g. sin, cos, tan, log).
; Does not check correctness of operation expression
(define (unary-op? expr)
  (and (list? expr) (= (length expr) 2)))

; Check whether a given expression is a binary operation (e.g. sum, production, exponentiation).
; Does not check correctness of operation expression
(define (binary-op? expr)
  (and (list? expr) (= (length expr) 3)))

; Check whether a given expression is a polyvariadic operation.
; Note that set of unary/binary operations belongs to set of polyvariadic operations
;  since any unary/binary operation is a valid polyvariadic operation.
; Does not check correctness of operation expression
(define (polyvariadic-op? expr)
  (and (list? expr) (> (length expr) 2)))


; ======= Conversions =======

; Convert polyvariadic expression to binary
; Example: '(+ 1 2 3) => '(+ (+ 1 2) 3)
; Only top-level operation is converted, operands still remain polyvariadic
(define (polyvariadic->binary expr)
  ; - result is the result obtained currently
  ; - op is the operation sign ('+, '*, 'log, etc.) obtained currently
  ; - terms is subexpression obtained currently
  (define (helper result op terms)
    (cond
      [(empty? terms) result]
      [(empty? result) (helper (list op (first terms) (second terms)) op (list-tail terms 2))]
      [else (helper (list op result (first terms)) op (rest terms))]))
  ; - result is empty by start, - op is the first element, - terms are the rest
  (helper '() (first expr) (rest expr)))


; Convert symbol to list of chars
(define (symbol->list symbol)
  (if (symbol? symbol)
      (string->list (symbol->string symbol))
      (raise-argument-error 'first-char "symbol" symbol)))


; ======= Symbol utilities =======
  
; Extract first character from symbol
(define (first-char symbol)
  (first (symbol->list symbol)))

; Check whether symbol contains only acceptable characters (alphabetical, numeric, #\_)
;  or symbol is blank
(define (contains-only-acceptable-chars? symbol)
  (define (helper chars result)
    (if (empty? chars)
        result
        (helper (rest chars) (and result
                                  (or
                                   (char-alphabetic? (first chars))
                                   (char-numeric? (first chars))
                                   (char=? (first chars) #\_))))))
  (helper (symbol->list symbol) #t))


; ======= Other utilities =======

; Check if given expression is:
;  '(* -1 <expr>) or '(* <expr> -1)
(define (negative-expr? expr)
  (and (product? expr) (or
                        (eq? (multiplier-1 expr) -1)
                        (eq? (multiplier-2 expr) -1))))

; Negate expression if it is negative (return multiplier that is not -1)
;  if not => just return it back
(define (positify-expr expr)
  (cond
    [(and (negative-expr? expr) (eq? (multiplier-1 expr) -1)) (multiplier-2 expr)]
    [(and (negative-expr? expr) (eq? (multiplier-2 expr) -1)) (multiplier-1 expr)]
    [else expr]))

; ======= Exceptions =======

; Raise argument error for binary operation
;  - name is name of function where error has occurred
;  - op is an unary operator sign ("sin", "cos", "tan", "log")
;  - given is a given expression 
(define (raise-argument-error-unary-op name op given)
  (raise-argument-error name (string-append "'(" op " <expr>)") given))

; Raise argument error for binary operation
;  - name is name of function where error has occurred
;  - op is a binary operator sign ("+", "*", "^")
;  - given is a given expression
(define (raise-argument-error-binary-op name op given)
  (raise-argument-error name (string-append "'(" op " <expr> <expr>)") given))

; Raise argument error for polyvariadic operation
;  - name is name of function where error has occurred
;  - given is a given expression
(define (raise-argument-error-polyvariadic-op name given)
  (raise-argument-error name "(valid-polyvariadic-op? '(<op> <expr>)) = #t" given))

; Raise argument error for logarithm operation if
;  its operand is less than or equal to 0.
;  - name is name of function where error has occurred
;  - given is a given expression
(define (raise-argument-error-log-op name given)
  (raise-argument-error name "(valid-log-op? <expr>): <expr> must be greater than 0" given))


; ======================================================================
; ========================= Exercise 1.1 ================================
; ======================================================================

; Check whether a given expression is a variable
;  where variable is a symbol which consists of characters that starts with char-alphabetic
;  and can contain char-aplhabetic, char-numeric or \#_ (underscore)
;  and is not a reserved mathematical operation keyword (sin, cos, tan, log, exp)
(define (variable? expr)
   (and
    (symbol? expr)
    (char-alphabetic? (first-char expr))
    (contains-only-acceptable-chars? expr)
    (not (member expr '(sin cos tan log exp)))))


; Check whether a given expression is a binary sum.
; Does not check correctness of summands' expressions
(define (sum? expr)
  (and (binary-op? expr) (eq? (first expr) '+)))

; Extract first summand from a sum
;  #raises argument error if expr is not a valid sum (@see (sum? expr) definition)
(define (summand-1 expr)
  (if (not (sum? expr))
      (raise-argument-error-binary-op 'summand-1 "+" expr) ; not a valid sum
      (list-ref expr 1)))

; Extract second summand from a sum.
;  #raises argument error if expr is not a valid sum (@see (sum? expr) definition)
(define (summand-2 expr)
  (if (not (sum? expr))
      (raise-argument-error-binary-op 'summand-2 "+" expr) ; not a valid sum
      (list-ref expr 2)))


; Check whether a given expression is a binary product.
; Does not check correctness of multipliers' expressions
(define (product? expr)
  (and (binary-op? expr) (eq? (first expr) '*)))

; Extract first multiplier from a product.
;  #raises argument error if expr is not a valid product (@see (product? expr) definition)
(define (multiplier-1 expr)
  (if (not (product? expr))
      (raise-argument-error-binary-op 'multiplier-1 "*" expr) ; not a valid product
      (list-ref expr 1)))

; Extract second multiplier from a product.
;  #raises argument error if expr is not a valid product (@see (product? expr) definition)
(define (multiplier-2 expr)
  (if (not (product? expr))
      (raise-argument-error-binary-op 'multiplier-2 "*" expr) ; not a valid product
      (list-ref expr 2)))


; ======================================================================
; ========================= Exercise 1.2 ================================
; ======================================================================

; Derivate variable expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivate-var expr var)
  (cond
    [(and (variable? expr) (eq? expr var)) '1] ; x/dx = 1
    [(variable? expr) '0])) ; y/dx = 0, where y is variable
  
; Derivate sum expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-sum expr var)
  (list
   '+
   (derivative (summand-1 expr) var)
   (derivative (summand-2 expr) var)))

; Derivate product expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-product expr var)
  (list
   '+
   (list '* (derivative (multiplier-1 expr) var) (multiplier-2 expr))
   (list '*  (multiplier-1 expr) (derivative (multiplier-2 expr) var))))

; Derivate exponentiation expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-expt expr var)
  (list
   '*
   (list '^ (base expr) (power expr))
   (list
    '+
    (list '* (derivative (power expr) var) (list 'log (base expr)))
    (list '* (power expr) (list '* (derivative (base expr) var))))))

; Derivate logarithm expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-log expr var)
  (list
   '*
   (derivative (second expr) var)
   (list '^ (second expr) -1)))

; Derivate sine expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-sin expr var)
  (list
   '*
   (derivative (second expr) var)
   (list 'cos (second expr))))

; Derivate cosine expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-cos expr var)
  (list
   '*
   (derivative (second expr) var)
   (list '* -1 (list 'sin (second expr)))))

; Derivate tangent expression with respect to var
; - var is variable with respect to that to compute derivative
(define (derivative-tan expr var)
  (list
   '*
   (derivative (second expr) var)
   (list '^ (list 'cos (second expr)) -2)))


; Compute a symbolic derivative of a given expression
;  with respect to a given variable.
; The result contains only unary and/or binary expression
; - expr is a given expression
; - var is a variable with respect to which to compute derivative
(define (derivative expr var)
   (cond
    [(not (variable? var))
     (raise-argument-error
      'derivative
      (string-append "(variable? " var ") = #t")
      expr)]
    [(number? expr) '0] ; c/dx = 0, where c = const
    [(variable? expr) (derivate-var expr var)]
    [(not (valid-polyvariadic-op? expr))
     (raise-argument-error-polyvariadic-op 'derivative expr)]
    [(sum? expr) (derivative-sum expr var)]
    [(product? expr) (derivative-product expr var)]
    [(expt? expr) (derivative-expt expr var)]
    [(log? expr) (derivative-log expr var)]
    [(sin? expr) (derivative-sin expr var)]
    [(cos? expr) (derivative-cos expr var)]
    [(tan? expr) (derivative-tan expr var)]
    [(polyvariadic-op? expr) (derivative (polyvariadic->binary expr) var)]))


; ======================================================================
; ========================= Exercise 1.3 ================================
; ======================================================================

; Simplify sum expression in following way:
; '(+ e 0) = 'e
; '(+ 0 e) = 'e
; '(+ c1 c2) = (+ c1 c2) where c1 and c2 are valid numbers
; '(+ e (* -1 e)) = 0
; '(+ (* -1 e) e) = 0
; '(+ (* e -1) e) = 0
; '(+ e (* e -1)) = 0
; else e = e
;
; Supports only binary sum.
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not a binary sum
(define (simplify-sum expr)
  (define left (summand-1 expr))
  (define right (summand-2 expr))
  (cond
    [(eq? left 0) right]
    [(eq? right 0) left]
    [(and (number? left) (number? right)) (+ left right)]
    [(and
      (negative-expr? left)
      (eq? (positify-expr left) right)) '0]
    [(and
      (negative-expr? right)
      (eq? (positify-expr right) left)) '0]
    [else expr]))

; Simplify product expression in following way:
; '(* 1 e) = e
; '(* e 1) = e
; '(* 0 e) = e
; '(* e 0) = 0
; '(* c1 c2) = (* c1 c2) where c1 and c2 are valid numbers
; '(* e e) = '(^ e 2)
; '(* (^ e1 e2) (^ e1 e3)) = '(e1 (+ e2 e3))
; '(* e1 (^e1 e2)) = '(^ e1 (+ e1 e2))
; '(* (^e1 e2) e1) = '(^ e1 (+ e1 e2))
; else e = e
;
; Supports only binary product.
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not a binary product
(define (simplify-product expr)
  (define left (multiplier-1 expr))
  (define right (multiplier-2 expr))
  (cond
    [(eq? left 1) right]
    [(eq? right 1) left]
    [(eq? left 0) '0]
    [(eq? right 0) '0]
    [(and (number? left) (number? right)) (* left right)]
    [(eq? left right) (list '^ left '2)]
    [(and (expt? left) (expt? right) (eq? (base left) (base right)))
     (list '^ (base left) (list '+ (power left) (power right)))]
    [(and (expt? left) (eq? (base left) right))
     (list '^ right (list '+ (power left) '1))]
    [(and (expt? right) (eq? (base right) left))
     (list '^ left (list '+ (power right) '1))]
    [else expr]))

; Simplify product expression in following way:
; '(^ c1 c2) = (^ c1 c2) where c1 and c2 are valid numbers
; '(^ (^ e1 e2) e3) = (^ e1 (* e2 e3))
; else e = e
;
; Supports only binary exponentiation.
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not a binary exponentioation
(define (simplify-expt expr)
  (define pow (power expr))
  (define bas (base expr))
  (cond
    [(expt? bas) (list '^ (base bas) (list '* pow (power bas)))]
    [(and (number? bas) (number? pow)) (expt bas pow)]
    [else expr]))

; Simplify logarithm expression in following way:
; '(log e) = (log e)
; else e = e
;
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not logarithm operation
(define (simplify-log expr)
  (cond
    [(not (log? expr))
     (raise-argument-error-unary-op 'simplify-log "log" expr)]
    [(number? (second expr)) (simplify-log '(log 3))(log (second expr))]
    [else expr]))

; Simplify sine expression in following way:
; '(sin e) = (sin e)
; else e = e
;
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not sine operation
(define (simplify-sin expr)
  (cond
    [(not (sin? expr))
     (raise-argument-error-unary-op 'simplify-sin "sin" expr)]
    [(number? (second expr)) (sin (second expr))]
    [else expr]))

; Simplify cosine expression in following way:
; '(cos e) = (cos e)
; else e = e
;
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not cosine operation
(define (simplify-cos expr)
  (cond
    [(not (cos? expr))
     (raise-argument-error-unary-op 'simplify-cos "cos" expr)]
    [(number? (second expr)) (cos (second expr))]
    [else expr]))

; Simplify tangent expression in following way:
; '(tan e) = (tan e)
; else e = e
;
; Supports only top-level simplification, operands are not evaluated.
;  #raises argument-error if given expression is not tangent operation
(define (simplify-tan expr)
  (cond
    [(not (tan? expr))
     (raise-argument-error-unary-op 'simplify-tan "tan" expr)]
    [(number? (second expr)) (tan (second expr))]
    [else expr]))


; Simplify a given expression with the rules that
;  correspond to simplification functions above.
; Supports only top-level simplification, operands are not evaluated.
; Supports only unary/binary operations simplification
;  #raises argument-error if given expression is not valid polyvariadic expression
(define (simplify-at-root expr)
  (cond
    [(or (number? expr) (variable? expr)) expr]
    [(not (valid-unary-binary-op? expr)) ; validation
     (raise-argument-error-polyvariadic-op 'simplify expr)]
    [(sum? expr) (simplify-sum expr)]
    [(product? expr) (simplify-product expr)]
    [(expt? expr) (simplify-expt expr)]
    [(log? expr) (simplify-log expr)]
    [(sin? expr) (simplify-sin expr)]
    [(cos? expr) (simplify-cos expr)]
    [(tan? expr) (simplify-tan expr)]))

; Simplify a given expression with the rules that
;  correspond to simplification functions above.
; Supports whole expression simplification, operands are evaluated.
;  #raises argument-error if given expression is not valid polyvariadic expression 
(define (simplify expr)
  (cond
    [(or (number? expr) (variable? expr)) expr]
    [(not (valid-polyvariadic-op? expr)) ; validation
     (raise-argument-error-polyvariadic-op 'simplify expr)]
    [(sum? expr)
     (simplify-at-root
      (list '+ (simplify (summand-1 expr)) (simplify (summand-2 expr))))]
    [(product? expr)
     (simplify-at-root
      (list '* (simplify (multiplier-1 expr)) (simplify (multiplier-2 expr))))]
    [(expt? expr)
     (simplify-at-root
      (list '^ (simplify (base expr)) (simplify (power expr))))]
    [(log? expr)
     (simplify-at-root
      (list 'log (simplify (second expr))))]
    [(sin? expr)
     (simplify-at-root
      (list 'sin (simplify (second expr))))]
    [(cos? expr)
     (simplify-at-root
      (list 'cos (simplify (second expr))))]
    [(tan? expr)
     (simplify-at-root
      (list 'tan (simplify (second expr))))]
    [(polyvariadic-op? expr) (simplify (polyvariadic->binary expr))]))


; ======================================================================
; ========================= Exercise 1.5 ================================
; ======================================================================

; Convert an expression into an infix form.
; Supports any polyvariadic operations (with 1, 2 and more arguments).
;  #raises argument error if expr is invalid or (contains invalid operations
(define (to-infix expr)
  (cond
    [(or (number? expr) (variable? expr)) expr]
    [(not (valid-polyvariadic-op? expr)) ; validation
     (raise-argument-error-polyvariadic-op 'to-infix expr)]
    [(unary-op? expr) (cons (first expr) (list (to-infix (second expr))))]
    [(binary-op? expr) (append (list (to-infix (second expr)))
                               (list (first expr))
                               (list (to-infix (third expr))))]
    [(polyvariadic-op? expr) (to-infix (polyvariadic->binary expr))]))


; ======================================================================
; ========================= Exercise 1.6 ================================
; ======================================================================

; PLEASE, CHECK Exercise 1.2 FOR LATEST VERSION OF (derivative expr)
; PLEASE, CHECK Exercise 1.3 FOR LATEST VERSION OF (simplify expr)


; Check whether a given expression is a exponentiation.
; Does not check correctness of operands' expressions
(define (expt? expr)
  (and (binary-op? expr) (eq? (first expr) '^)))

; Extract first base from exponentiation
;  #raises argument error if expr is not
;   a valid exponentiaion (@see (expt? expr) definition)
(define (base expr)
  (if (not (expt? expr))
      (raise-argument-error-binary-op 'base "^" expr) ; not a valid exponentiation
      (list-ref expr 1)))

; Extract first power from exponentiation
;  #raises argument error if expr is not
;   a valid exponentiation (@see (expt? expr) definition)
(define (power expr)
  (if (not (expt? expr))
      (raise-argument-error-binary-op 'power "^" expr) ; not a valid exponentiation
      (list-ref expr 2)))


; Check whether a given expression is a sine
; Does not check correctness of operand's expressions
(define (sin? expr)
  (and (unary-op? expr) (eq? (first expr) 'sin)))

; Check whether a given expression is a cosine
; Does not check correctness of operand's expressions
(define (cos? expr)
  (and (unary-op? expr) (eq? (first expr) 'cos)))

; Check whether a given expression is a tangent
; Does not check correctness of operand's expressions
(define (tan? expr)
  (and (unary-op? expr) (eq? (first expr) 'tan)))

; Check whether a given expression is a logarithm
; Does not check correctness of operand's expressions
(define (log? expr)
  [and (unary-op? expr) (eq? (first expr) 'log)])


; ======================================================================
; ========================= Exercise 1.7 ================================
; ======================================================================

; PLEASE, CHECK Exercise 1.2 FOR LATEST VERSION OF (derivative expr)
; PLEASE, CHECK Exercise 1.3 FOR LATEST VERSION OF (simplify expr)


; Check whether a given expression is a polyvariadic sum
(define (polyvariadic-sum? expr)
  (and (polyvariadic-op? expr) (eq? (first expr) '+)))

; Check whether a given expression is a polyvariadic product
(define (polyvariadic-product? expr)
  (and (polyvariadic-op? expr) (eq? (first expr) '*)))


; ======================================================================
; ========================= Exercise 1.8 ================================
; ======================================================================

; Returns a (sorted) list of distinct variables used in a given expression.
; Supports polyvariadic operations
(define (variables-of expr)
  (define (helper expr result)
    (cond
      [(number? expr) result]
      [(variable? expr) (cons expr result)]
      [(not (valid-polyvariadic-op? expr)) ; validation
       (raise-argument-error-polyvariadic-op 'variables-of  expr)]
      [(unary-op? expr)        (append result
                                       (helper (second expr) result))]
      [(binary-op? expr)       (append result
                                       (helper (second expr) result)
                                       (helper (third expr) result))]
      [(polyvariadic-op? expr) (append result
                                       (helper (polyvariadic->binary expr) result))]))
      
  (sort (remove-duplicates (helper expr '())) string<? #:key symbol->string))

