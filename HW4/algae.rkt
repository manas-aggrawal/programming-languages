#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { < <ALGAE> <ALGAE> }
               | { = <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
               | True
               | False
               | { with { <id> <ALGAE> } <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | <id>
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num    Number]
  [Add    (Listof ALGAE)]
  [Mul    (Listof ALGAE)]
  [Sub    ALGAE (Listof ALGAE)]
  [Div    ALGAE (Listof ALGAE)]
  [Less   ALGAE ALGAE]
  [Equal  ALGAE ALGAE]
  [LessEq ALGAE ALGAE]
  [Bool   Boolean]
  [Id     Symbol]
  [If     ALGAE ALGAE ALGAE]
  [With   Symbol ALGAE ALGAE])

(: parse-sexpr : Sexpr -> ALGAE)
;; parses s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)      (Num n)]
    [(symbol: 'True)  (Bool #t)]
    [(symbol: 'False) (Bool #f)]
    [(symbol: name)  (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ args ...)     (Add (parse-sexprs args))]
    [(list '* args ...)     (Mul (parse-sexprs args))]
    [(list '- fst args ...) (Sub (parse-sexpr fst) (parse-sexprs args))]
    [(list '/ fst args ...) (Div (parse-sexpr fst) (parse-sexprs args))]
    [(list '< lhs rhs )     (Less (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '= lhs rhs )     (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '<= lhs rhs )    (LessEq (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'if c t f)       (If (parse-sexpr c) (parse-sexpr t) (parse-sexpr f))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2', `E3' are <ALGAE>s, `x' is some <id>, `y' is a
   *different* <id>, `B' is a True/False`)
      N[v/x]                = N
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      {< E1 E2 }[v/x]       = {< E1[v/x] E2[v/x]}
      {= E1 E2 }[v/x]       = {= E1[v/x] E2[v/x]}
      {<= E1 E2 }[v/x]      = {<= E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      B[v/x]                = B
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {if E1 E2 E3}[v/x]    = {if E1[v/x] E2[v/x] E3[v/x]}
|#

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)          expr]
    [(Add args)       (Add (substs* args))]
    [(Mul args)       (Mul (substs* args))]
    [(Sub fst args)   (Sub (subst* fst) (substs* args))]
    [(Div fst args)   (Div (subst* fst) (substs* args))]
    [(Less lhs rhs)   (Less (subst* lhs) (subst* rhs))]
    [(Equal lhs rhs)  (Equal (subst* lhs) (subst* rhs))]
    [(LessEq lhs rhs) (LessEq (subst* lhs) (subst* rhs))]
    [(Bool b)         (Bool b)]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
               bound-body
               (subst* bound-body)))]
    [(If c t f) (If (subst* c) (subst* t) (subst* f))]))

#| Formal specs for `eval':
     eval(N)            = N
     eval({+ E ...})    = evalN(E) + ...
     eval({* E ...})    = evalN(E) * ...
     eval({- E})        = -evalN(E)
     eval({/ E})        = 1/evalN(E)
     eval({- E1 E ...}) = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...}) = evalN(E1) / (evalN(E) * ...)
     eval({< E1 E2}) = evalN(E1) / (evalN(E) * ...)
     eval(id)           = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     eval({if E1 E2 E3}) =  if evalB(E1) is true : eval(E2),
                            if evalB(E1) is false: eval(E3) // TODO: Check syntax
     evalN(E) = eval(E) if it is a number, error otherwise
     evalB(E) = eval(E) if it is a boolean error otherwise
|#

(: eval-number : ALGAE -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr)
  (let ([result (eval expr)])
    (if (number? result)
        result
        (error 'eval-number "need a number when evaluating ~s, but got ~s"
               expr result))))

(: eval-boolean : ALGAE -> Boolean)
(define (eval-boolean expr)
  (let ([result (eval expr)])
    (if (boolean? result)
        result
        (error 'eval-boolean "need a boolean when evaluating ~s, but got ~s"
               expr result))))

(: value->algae : (U Number Boolean) -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number? val) (Num val)]
        [(boolean? val) (Bool val)]
        ;; Note: a `cond' doesn't make much sense now, but it will when
        ;; we extend the language with booleans.  Also, since we use
        ;; Typed Racket, the type checker makes sure that this function
        ;; is never called with something that is not in its type, so
        ;; there's no need for an `else' branch like
        ;;     [else (error 'value->algae "unexpected value: ~s" val)]
        ;; (Strictly speaking, there's no need for the last predicate
        ;; (which is the only one here until you extend this), but it's
        ;; left in for clarity.)
        #|... more code should be here ...|#))

(: eval : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers
;; TODO: Alignment
(define (eval expr)
  (cases expr
    [(Num n)  n]
    [(Bool b) b]
    [(Add args)
     (if (null? args)
         0
         (foldl + 0 (map eval-number args)))]
    [(Mul args)
     (if (null? args)
         1
         (foldl * 1 (map eval-number args)))]
    [(Sub fst args)
     (if (null? args)
         (- (eval-number fst)) ; Negate the single argument if no others
         (- (eval-number fst) (foldl + 0 (map eval-number args))))]
    [(Div fst args)
     (if (null? args)
         (eval-number fst) ; Division by one value returns that value
         (let ([denom (foldl * 1 (map eval-number args))])
           (if (zero? denom)
               (error 'eval "Division by zero!")
               (/ (eval-number fst) denom))))]
    [(Less lhs rhs)   (< (eval-number lhs) (eval-number rhs))]
    [(Equal lhs rhs)  (= (eval-number lhs) (eval-number rhs))]
    [(LessEq lhs rhs) (<= (eval-number lhs) (eval-number rhs))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(If c t f) (if (eval-boolean c) (eval t) (eval f))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (U Number Boolean))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests (for simple expressions)
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{+ 5 5 5 5}") => 20)
(test (run "{- 10 5 3 1}") => 1)
(test (run "{+}") => 0)
(test (run "{* 5 5}") => 25)
(test (run "{* 5 5 5 5}") => 625)
(test (run "{- 10}") => -10)
(test (run "{*}") => 1)
(test (run "{/ 5 6}") => 5/6)
(test (run "{/ 20 5 2}") => 2)
(test (run "{/ 20}") => 20)
(test (run "{/ 20 0}") =error> "Division by zero")
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x {* 5 6}} {* x x}}") => 900)
(test (run "{with {x {/ 20 10}} {/ x x}}") => 1)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{5 6 /}") =error> "parse-sexpr: bad syntax in (5 6 /)")
(test (run "{with {5 5} {with {x x} x}}") =error>
      "parse-sexpr: bad `with' syntax in (with (5 5) (with (x x) x))")

;; Relational operators
(test (run "{< 3 5}") => #t)
(test (run "{= 5 5}") => #t)
(test (run "{<= 5 5}") => #t)

;; Booleans
(test (run "True") => #t)
(test (run "False") => #f)
;;
;; if expressions
(test (run "{if True 4 5}") => 4)
(test (run "{if False 4 5}") => 5)
(test (run "{if {< 2 3} {+ 1 2} {* 3 4}}") => 3)

;; ;; Error: non-boolean condition
(test (run "{if 5 1 2}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")

(test (run "{< True False}") =error>
      "eval-number: need a number when evaluating (Bool #t), but got #t")

;;
;; ;; Error: invalid relational operator arguments
(test (run "{< 1}") =error> "bad syntax in (< 1)")
(test (run "{= 1 2 3}") =error> "bad syntax in (= 1 2 3)")

;; Using with and If together:
(test (run "{with {x True} {if x 5 10}}") => 5)
(test (run "{with {x False} {if x 5 10}}") => 10)
(test (run "{with {x 3} {if {< x 5} 1 0}}") => 1)
(test (run "{with {y 10} {if {= y 10} {with {z 5} {+ z y}} {with {z 2} {* z y}}}}") => 15)
(test (run "{with {a 4} {+ a {if {<= a 5} 1 -1}}}") => 5)
(test (run "{with {x 5} {if x 1 2}}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")
(test (run "{with {x 8} {with {y {if {< x 10} 2 3}} {* x y}}}") => 16)
(test (run "{with {a 7} {with {b {if {= a 7} {+ 1 2} {* 3 4}}} {/ b 2}}}") => 3/2)
(test (run "{with {flag False} {if flag {+ 1 2} {* 3 4}}}") => 12)
(test (run "{with {x 5} {if True y 10}}") =error> "eval: free identifier: y")
