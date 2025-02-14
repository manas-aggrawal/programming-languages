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
               | { with { <id> <ALGAE> } <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { not <ALGAE> }
               | { and <ALGAE> <ALGAE> }
               | { or <ALGAE> <ALGAE> }
               | <id>
               | True
               | False
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

(: Not : ALGAE -> ALGAE)
;; Helper to generate binding for Not operation
(define (Not expr)
  (If expr (Bool #f) (Bool #t)))

(: And : ALGAE ALGAE -> ALGAE)
;; Helper to generate binding for And operation
(define (And arg1 arg2)
  (If arg1 arg2 (Bool #f)))

(: Or : ALGAE ALGAE -> ALGAE)
;; Helper to generate binding for Not operation
(define (Or arg1 arg2)
  (If arg1 (Bool #t) arg2))

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
    [(list 'if c t f)       (If (parse-sexpr c)
                                (parse-sexpr t) (parse-sexpr f))]
    [(list 'not arg)        (Not (parse-sexpr arg))]
    [(list 'and arg1 arg2)  (And (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'or arg1 arg2)   (Or  (parse-sexpr arg1) (parse-sexpr arg2))]
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
     eval(N)                 = N
     eval({+ E ...})         = evalN(E) + ...
     eval({* E ...})         = evalN(E) * ...
     eval({- E})             = -evalN(E)
     eval({/ E})             = 1/evalN(E)
     eval({- E1 E ...})      = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...})      = evalN(E1) / (evalN(E) * ...)
     eval({< E1 E2})         = if evalN(E1) is less than evalN(E2) True
                               otherwise False
     eval({= E1 E2})         = if evalN(E1) is equal to evalN(E2) True
                               otherwise False
     eval({<= E1 E2})        = if evalN(E1) is lesser than or equal to
                               evalN(E2) True otherwise False
     eval(id)                = error!
     eval({with {x E1} E2})  = eval(E2[eval(E1)/x])
     eval({if E1 E2 E3})     = if evalB(E1) is True eval(E2) otherwise eval(E3)
     eval({not E})           = eval({if E False True})
     eval({and E1 E2})       = eval({if E1 {if E2 True False} False})
     eval({or E1 E2})        = eval({if E1 True {if E2 True False}})
     evalN(E)                = eval(E) if it is a number, error otherwise
     evalB(E)                = eval(E) if it is a boolean error otherwise
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
        [(boolean? val) (Bool val)]))

(: eval : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers
;; TODO: Alignment
(define (eval expr)
  (cases expr
    [(Num n)    n]
    [(Bool b)   b]
    [(Add args) (if (null? args) 0 (foldl + 0 (map eval-number args)))]
    [(Mul args) (if (null? args) 1 (foldl * 1 (map eval-number args)))]
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
    [(If c t f)       (if (eval-boolean c) (eval t) (eval f))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name)        (error 'eval "free identifier: ~s" name)]))


(: run : String -> (U Number Boolean))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests (for simple expressions)
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{+ 5 5 5 5}") => 20)
(test (run "{+}") => 0)
(test (run "{* 5 5}") => 25)
(test (run "{* 5 5 5 5}") => 625)
(test (run "{*}") => 1)
(test (run "{- 10}") => -10)
(test (run "{- 10 5}") => 5)
(test (run "{- 10 5 3 1}") => 1)
(test (run "{-}") =error> "parse-sexpr: bad syntax in (-)")
(test (run "{/ 5 6}") => 5/6)
(test (run "{/ 20 5 2}") => 2)
(test (run "{/ 20}") => 20)
(test (run "{/ 720 2 3 4 5 6}") => 1)
(test (run "{/ 20 0}") =error> "Division by zero")
(test (run "{/}") =error> "parse-sexpr: bad syntax in (/)")
(test (run "{+ {+ 5 5 6} {- 2 {/ 20 10}} {* 2 4 5}}") => 56)
(test (run "{* {- 5 5 6} {- 2 {+ 20 10}} {/ 30 2 3}}") => 840)

;; tests using with
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x {+ 5 5}} {+ x x x x x x x}}") => 70)
(test (run "{with {x {- 10 5}} {- x x x x x x x}}") => -25)
(test (run "{with {x {* 5 6}} {* x x}}") => 900)
(test (run "{with {x {* 4 5}} {* x x x x x}}") => 3200000)
(test (run "{with {x {/ 20 10}} {/ x x}}") => 1)
(test (run "{with {x {/ 20 10}} {/ 20000 x x x x x}}") => 625)
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

;; tests for less, lesseq and equal
(test (run "{< 3 5}") => #t)
(test (run "{= 5 5}") => #t)
(test (run "{<= 5 5}") => #t)
(test (run "{< {+ 4 6} 5}") => #f)
(test (run "{< {+ 4 6} {- 100 5}}") => #t)
(test (run "{< {+ 4 6} {- 100 {* 10 2}}}") => #t)
(test (run "{< {+ 4 6} {- 100 {* {/ 40 4} 2}}}") => #t)
(test (run "{= {+ 4 6} 5}") => #f)
(test (run "{= {+ 40 60} {- 120 20}}") => #t)
(test (run "{= {+ 40 60} {- 120 {* 10 2}}}") => #t)
(test (run "{= {+ 40 60} {- 120 {* {/ 40 4} 2}}}") => #t)
(test (run "{<= {+ 4 6} 5}") => #f)
(test (run "{<= {+ 40 60} {- 140 20}}") => #t)
(test (run "{<= {+ 40 60} {- 140 {* 10 2}}}") => #t)
(test (run "{<= {+ 40 60} {- 140 {* {/ 40 4} 2}}}") => #t)
(test (run "{< 1}") =error> "bad syntax in (< 1)")
(test (run "{<= 1}") =error> "bad syntax in (<= 1)")
(test (run "{< True 4}") =error>
      "eval-number: need a number when evaluating (Bool #t), but got #t")
(test (run "{<= True 4}") =error>
      "eval-number: need a number when evaluating (Bool #t), but got #t")
(test (run "{= 4 False}") =error>
      "eval-number: need a number when evaluating (Bool #f), but got #f")
(test (run "{= 1 2 3}") =error> "bad syntax in (= 1 2 3)")

;; tests for bool operator
(test (run "True") => #t)
(test (run "False") => #f)
(test (run "{< True False}") =error>
      "eval-number: need a number when evaluating (Bool #t), but got #t")

;; tests for if
(test (run "{if True 4 5}") => 4)
(test (run "{if False 4 5}") => 5)
(test (run "{if {< 2 3} {+ 1 2} {* 3 4}}") => 3)
(test (run "{if {< {+ 7 8 9} {* 2 2 2}} {+ 1 2} {* 3 4}}") => 12)
(test (run "{if {< {if {<= 5 5} 2 4} 3} {+ 1 2} {* 3 4}}") => 3)

(test (run "{if 5 1 2}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")

;; tests using with and if
(test (run "{with {x True} {if x 5 10}}") => 5)
(test (run "{with {x False} {if x 5 10}}") => 10)
(test (run "{with {x 3} {if {< x 5} 1 0}}") => 1)
(test (run "{with {y 10}
            {if {= y 10} {with {z 5} {+ z y}} {with {z 2} {* z y}}}}") => 15)
(test (run "{with {a 4} {+ a {if {<= a 5} 1 -1}}}") => 5)
(test (run "{with {x 5} {if x 1 2}}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")
(test (run "{with {x 8} {with {y {if {< x 10} 2 3}} {* x y}}}") => 16)
(test (run "{with {a 7}
            {with {b {if {= a 7} {+ 1 2} {* 3 4}}} {/ b 2}}}") => 3/2)
(test (run "{with {flag False} {if flag {+ 1 2} {* 3 4}}}") => 12)
(test (run "{with {x 5} {if True y 10}}") =error> "eval: free identifier: y")
(test (run "{if {with {x 5} {< x 10}} 25 30}") => 25)

;; tests for NOT, AND and OR 
(test (run "{not True}") => #f)
(test (run "{not False}") => #t)
(test (run "{and True True}") => #t)
(test (run "{and True False}") => #f)
(test (run "{and {or False True} False}") => #f)
(test (run "{and {or False True} True}") => #t)
(test (run "{and {or False True} {not False}}") => #t)
(test (run "{or {and False True} {not False}}") => #t)
(test (run "{not {and {or False False} True}}") => #t)
(test (run "{and {< 2 3} True}") => #t)
(test (run "{and {<= 200 200} True}") => #t)
(test (run "{or {<= 200 200} False}") => #t)
(test (run "{or {< 200 300} False}") => #t)
(test (run "{or {< 2000 300} False}") => #f)
(test (run "{or True False}") => #t)
(test (run "{or False True}") => #t)
(test (run "{or False False}") => #f)
(test (run "{or True 5}") => #t)
(test (run "{if {and {< 5 10} {<= 7 20}} {+ 5 5 5 5} {- 4 4 4}}") => 20)
(test (run "{if {or {< 5 10} {<= 70 20}} {+ 5 5 5 5} {- 4 4 4}}") => 20)
(test (run "{if {not {< 5 10}} {+ 5 5 5 5} {- 4 4 4}}") => -4)
(test (run "{if {and {with {x 5} {< x 10}} {<= 7 20}}
                {+ 5 5 5 5}
                {- 4 4 4}}") => 20)
(test (run "{if {or {with {x 5} {< x 1}} {<= 7 20}}
                {+ 5 5 5 5}
                {- 4 4 4}}") => 20)
(test (run "{if {not {with {x 5} {< x 1}}}
                {+ 5 5 5 5}
                {- 4 4 4}}") => 20)

(test (run "{and 1 True}") =error>
      "eval-boolean: need a boolean when evaluating (Num 1), but got 1")
(test (run "{and True 5}") => 5)
(test (run "{if {and True 1} 2 3}") =error>
      (string-append "eval-boolean: need a boolean when "
                     "evaluating (If (Bool #t) (Num 1) (Bool #f)), but got 1"))

(test (run "{not 5}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")
(test (run "{and 5 True}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")
(test (run "{or 5 False}") =error>
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")

(define minutes-spent 300)
