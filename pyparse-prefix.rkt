        ; Anil Kumar Konasale krishna
        ;   akumarkk@cs.utah.edu 
        ;       UID - u0939372

        ; Sahana Sandeep
        ;   sahana@cs.utah.edu
        ;       UID - u0932424


#lang racket

(require parser-tools/cfg-parser)

(require parser-tools/lex)


;; Helpers:
(define (unzip/callback lst k)
  (match lst
    ['() (k '() '())]
    [(cons (list a b) tl)
     (unzip/callback tl (λ (as bs)
       (k (cons a as) (cons b bs))))]))



;; Lexer definitions:
(define-tokens ID (NAME))

(define-tokens LIT (STRING NUMBER))

(define-empty-tokens PUNCT (+ - * ** / // % << >> & \| ^ ~ < > <= >= == !=
                      <> ... ; had to add these due to bug/omission in lexer spec
                      |(| |)| |[| |]| |{| |}| |,| : |.| |;| @ = -> += -= *= /= //= %= &= \|= ^= >>= <<= **=))


(define-empty-tokens KEYWORD (False class finally is return None continue for lambda try
                        True def from nonlocal while and del global not with as elif if 
                        or yield assert else import pass break except in raise))

(define-empty-tokens SPECIAL (ENDMARKER NEWLINE INDENT DEDENT LIT EOF))



;; Auxiliary definitions:


;; Supply auxiliary helpers here, like process-trailers:

(define (process-trailers base trailers)
 (match trailers
  ['()      base]
  [(cons (cons "[" a) rest) `(Subscript, base,a)]))


;; You may want to put definitions here rather than defining
;; them in the grammar itself.

(define (process-binops base ops)
(match ops
['()
base]
[(cons (list op exp) rest)
(process-binops `(BinOp, base ,
		 (cond [(equal? op "+") `Add] 
		       [(equal? op "-") `Sub]
		       [(equal? op "*") `Mult]
		       [(equal? op "/") `Div]
		       [(equal? op "%") `Mod]
		       [(equal? op "//") `FloorDiv]
		       [(equal? op "<<") `LShift]
		       [(equal? op ">>") `RShift]
		       [(equal? op "&") `BitAnd]
		       [(equal? op "^") `BitXor]
		       [(equal? op "|") `BitOr]), 
		  exp) rest)]))

(define (process-augassign base ops)
(begin
(match ops
['()
base]
[(cons op exp)
 (begin
      (if (equal? (car op) "=") (process-assign base ops)
                 `(AugAssign, base ,
                 (cond [(equal? op "+=") `Add]
                       [(equal? op "-=") `Sub]
                       [(equal? op "*=") `Mult]
                       [(equal? op "/=") `Div]
                       [(equal? op "%=") `Mod]
                       [(equal? op "//=") `FloorDiv]
                       [(equal? op "<<=") `LShift]
                       [(equal? op ">>=") `RShift]
                       [(equal? op "&=") `BitAnd]
                       [(equal? op "^=") `BitXor]
                       [(equal? op "|=") `BitOr]),
                  (car exp)))) ]
)))

(define (process-unops base ops)
(match ops
['()
  base] 
[_ (process-unops `(UnaryOp,
(match base
["+" `UAdd]
["-" `USub]
["~" `Invert]
["not" `Not]), ops) '())]))


(define (process-level base)

  (if (equal? base null)
    0
  (match (car base)
            ["." (+ (process-level (cdr base)) 1)]

            ["..." (+ (process-level (cdr base)) 3)])))



(define (process-dotted-names base variables)
  ;(display "Base-")
  ;(display base)
  ;(newline)
  ;(display "Varible-")
  ;(display variables)
  ;(newline)

  (match variables
   ['()
    base]

   [(cons (list comma var) rest)
    (begin
      (set! base (append base (list  var)))
      (process-dotted-names base  rest))]))



(define (process-globals base variables)
 ; (display "Base-")
  ;(display base)
 ; (newline)
 ; (display "Varible-")
 ; (display variables)
 ; (newline)

  (match variables
   ['()
    base]

   [(cons (list comma var) rest)
    (begin
      (set! base (append base (list (string->symbol var))))
      (process-globals base  rest))]))



(define (process-dots base variable)
  (match variable
  ['()
   (begin
     (set! base (string->symbol base))
      base)]

  [(cons (list dot var) rest)
   (begin
    (set! base (string-append base dot))
    (set! base (string-append base var))
    (process-dots base rest))]))
    ;(process-dots `,base,dot,var rest))]))



(define (process-as arg variable)
  ;(display "Debugging- ")
  ;(display arg)
  ;(newline)
  ;(display variable)
  ;(newline)
  (match variable
         ['()
          (arg)]

         [(cons as var)
            `(, arg ,var)]))



(define attr (list 'Attribute))
(define first 1)

(define (process-dotted base variables)
  ;(display "Base-")
  ;(display base)
  ;(newline)
  ;(display "Varible-")
  ;(display variables)
  ;(newline)

  (match variables
   ['()
    (begin
      (set! first 1)
        base)]

   [(cons (list comma var) rest)
    (begin
      (if (equal? first 1)
        (begin
            (set! base (append (list 'Name) base))
            (set! first 0)
   ;         (display "Base is now - ");
    ;        (display base)
     ;       (newline)
            )
        (void))

      (set! base (reverse (cons base attr)))
     ; (display "debug base")
     ; (display base)
     ; (newline)
      (set! base (append base (list (string->symbol var))))
     ; (display "debug base1")
     ; (display base)
     ; (newline) 
      (process-dotted base  rest))]))

(define (process-testlist arg1 arg2)
(match arg2
['()
arg1]
[_
        (begin
        (set! arg1 (append  arg1 (list (car arg2))))
        (process-testlist arg1 (rest arg2)))]))

(define tar '(targets))
(define val '())

(define (process-assign base ops)
(begin

(match ops
['()
(begin
    (set! base `(Assign, tar, `(value, val)))
 base)]
[(cons (list op exp) rest)
                   (begin
                   ;(display rest)

                   (set! tar (append tar (list base)))
                   (set! val exp)
                   ;(set! tar (append tar val))
                   (process-assign exp rest))])))

(define opers '(ops))
(define comps '(comparators))
;(define first 0)
(define (process-compops base ops)
;(begin
 ; (cond [(equal? first 0) ((set! base `(Compare, `(left, base), (process-compops '() ops)))
  ;(set! first 1))
  ;])
(match ops
['()
(begin
    (set! base (append `(,opers) `(,comps)))
    base)]

[(cons (list op exp) rest)
                   (begin
		   ;(display rest)
		   (set! opers (append opers (list op)))
		   (set! comps (append comps `(,exp)))
		   (process-compops '() rest))]))

;; The parser:
(define pyparse
  (cfg-parser
   
   (tokens ID LIT PUNCT KEYWORD SPECIAL)
   
   (end EOF)
  
   ; ATTENTION: To support working "bottom-up" through the grammar,
   ; the start symbol is set to `power` instead of `file_input`.
   ; You should change the start symbol as you move up the kinds
   ; of expressions.
   (start file_input)
   ;(start test)
   
   (error (λ (tok-ok? tok-name tok-value)
            (if tok-ok?
                (error (format "Unexpected token: ~a ~a" tok-name tok-value))
                (error (format "Invalid token: " ~a)))))
   
   (grammar


