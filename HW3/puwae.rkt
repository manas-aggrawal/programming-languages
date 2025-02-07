;; ** The PUWAE interpreter

#lang pl 03

#| BNF for the PUWAE language:
     <PUWAE> ::= <num>
             | { + <PUWAE> <PUWAE> }
             | { - <PUWAE> <PUWAE> }
             | { * <PUWAE> <PUWAE> }
             | { / <PUWAE> <PUWAE> }
             | { with { <id> <PUWAE> } <PUWAE> }
             | <id>
             | { post <POST> ... }

     <POST>  ::= <PUWAE> | + | - | * | /
|#

(define-type PostfixItem = (U PUWAE '+ '- '* '/))

;; PUWAE abstract syntax trees
(define-type PUWAE
  [Num  Number]
  [Add  PUWAE PUWAE]
  [Sub  PUWAE PUWAE]
  [Mul  PUWAE PUWAE]
  [Div  PUWAE PUWAE]
  [Id   Symbol]
  [With Symbol PUWAE PUWAE]
  [Post (Listof PostfixItem)])

(: parse-post-item : Sexpr -> PostfixItem)
;; parse an s-expression to a post-item
(define (parse-post-item s)
  (match s
    ['+ '+]
    ['- '-]
    ['* '*]
    ['/ '/]
    [else (parse-sexpr s)]))

(: parse-sexpr : Sexpr -> PUWAE)
;; parses s-expressions into PUWAEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(cons 'post more) (Post (map parse-post-item more))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> PUWAE)
;; parses a string containing a PUWAE expression to a PUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: subst : PUWAE Symbol PUWAE -> PUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (: post-subst : PostfixItem -> PostfixItem)
  (define (post-subst item)
    (if (symbol? item) item (subst item from to)))
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Post items) (Post (map post-subst items))]))

(: post-eval : (Listof PostfixItem) (Listof Number) -> Number)
;; Helper function eval which uses a stack to evaluate a post expression.
;; This is done by popping two elements in the stack when an operator is
;; encountered. The result is pushed back on to the stack for further
;; evaluation. When the stack contains only one element(base case), we return
;; with that as the result.
(define (post-eval items stack)
  (if (null? items)
      (match stack
        [(list result) result]
        [else (error 'post-eval
                     "post expression doesn't evaluate to a single number: ~s"
                     stack)])
      (let ([1st (first items)]
            [more (rest items)])
        (: pop2-and-apply : (Number Number -> Number) -> Number)
        (define (pop2-and-apply op)
          (match stack
            [(list a b remaining ...)
             (post-eval more (cons (op b a) remaining))]
            [else (error 'post-eval
                         "not enough operands for operator: ~s"
                         op)]))
        (cond
          [(eq? '+ 1st) (pop2-and-apply +)]
          [(eq? '- 1st) (pop2-and-apply -)]
          [(eq? '* 1st) (pop2-and-apply *)]
          [(eq? '/ 1st) (pop2-and-apply /)]
          [else ; has to be a PUWAE expression
           (post-eval more (cons (eval 1st) stack))]))))

(: eval : PUWAE -> Number)
;; evaluates PUWAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Post more) (post-eval more '())]))

(: run : String -> Number)
;; evaluate a PUWAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)

(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{with {x} y}")
      =error>
      "parse-sexpr: bad `with' syntax in (with (x) y)")
(test (run "{* {with x y}}")
      =error>
      "parse-sexpr: bad syntax in (* (with x y))")
(test (run "{with {x 5} {* x 10}}") => 50)
(test (run "{with {x 5} {/ 100 5}}") => 20)

(test (run "{post 1 2 +}") => 3)
(test (run "{post 2 1 -}") => 1)
(test (run "{post 2 5 *}") => 10)
(test (run "{post 54 18 /}") => 3)
(test (run "{post 1 2 + 4 *}") => 12)
(test (run "{post 1 2 + 4 * 2 -}") => 10)
(test (run "{post 1 2 + 4 * 2 - 5 /}") => 2)
(test (run "{post 1 {post 3 5 +} +}") => 9)
(test (run "{post {post 5 1 -} {post 3 5 +} *}") => 32)
(test (run "{post 1}") => 1)
(test (run "{post 1 2}")
      =error>
      "post-eval: post expression doesn't evaluate to a single number: (2 1)")
(test (run "{post 1 +}")
      =error>
      "post-eval: not enough operands for operator: #<procedure:+>")
(test (run "{post {+ 1 3} 2 +}") => 6)

(test (run "{with {x 5} {post x 15 +}}") => 20)
(test (run "{with {x {post 20 10 -}} {post x 15 +}}") => 25)
(test (run "{with {x {post 5 1 2 + 4 * + 3 -}} {post x 2 /}}") => 7)
(test (run "{with {x {post 20 10 -}}
                  {post {post x 20 *} {post x 15 +}/}}") => 8)
(test (run "{with {x 5} {post x 2 3}}")
      =error>
      (string-append "post-eval: post expression doesn't "
                     "evaluate to a single number: (3 2 5)"))
(test (run "{with {x 5} {post x +}}")
      =error>
      "post-eval: not enough operands for operator")
(test (run "{with {x 5} {post y 2 +}}") =error> "free identifier")

(test (run "{post {with {x 5} {+ x 3}} {with {y 2} {* y 4}} -}") => 0)
(test (run "{with {x {post 1 2 +}}
                  {with {y {post 3 4 +}}
                        {post x y * {with {z 2} {+ z 3}} +}}}") => 26)

;; Decimal division results
(test (run "{/ 5 2}") => 5/2)
(test (run "{post 5 2 /}") => 5/2)
;; Multiple levels of nesting
(test (run "{with {x 1} {with {y 2} {with {z 3} {+ x {+ y z}}}}}") => 6)
(test (run "{with {x {with {y 2} {+ y 3}}} {+ x 1}}") => 6)

;; Shadowing in deeply nested expressions
(test (run "{with {x 1} {with {x 2} {with {x 3} x}}}") => 3)
(test (run "{with {x 1} {+ x {with {x 2} {+ x {with {x 3} x}}}}}") => 6)
;; Mixed operations with nesting
(test (run "{post 1 2 + 3 4 + *}") => 21)
(test (run "{post 10 5 / 2 * 3 +}") => 7)

;; Postfix with nested expressions
(test (run "{post {with {x 5} {+ x 2}} {with {y 3} {* y 2}} +}") => 13)
(test (run "{post {post 1 2 +} {post 3 4 +} *}") => 21)
;; Invalid syntax
(test (run "{with}") =error> "bad `with' syntax")
(test (run "{post}")
      =error>
      "post-eval: post expression doesn't evaluate to a single number: ()")
(test (run "{with {x} {+ x 1}}") =error> "bad `with' syntax")

;; Invalid operations
(test (run "{+ {post 1 2 +} {post}}")
      =error>
      "post-eval: post expression doesn't evaluate to a single number: ()")
(test (run "{with {x {post + -}} x}")
      =error>
      "post-eval: not enough operands for operator: #<procedure:+>")

;; Unbound variables
(test (run "{with {x y} x}") =error> "free identifier")
(test (run "{with {x {+ y 1}} x}") =error> "free identifier")
;; Combining arithmetic and postfix
(test (run "{+ {post 1 2 +} {* 3 4}}") => 15)
(test (run "{with {x {post 1 2 +}} {post x {* 2 3} +}}") => 9)

;; Complex nested expressions
(test (run "{with {x {post 1 2 +}}
            {with {y {+ x 3}}
                  {post x y * {with {z {* 2 x}} {+ z 1}} +}}}") => 25)

(define minutes-spent 300)
