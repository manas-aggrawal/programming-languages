;; ** The Brang interpreter, using environments

#lang pl 06

#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> <id> ... } <BRANG> }
            | { call <BRANG> <BRANG> <BRANG> ... }

Evaluation rules:
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval({CWith E1 E2},env) = eval(E2, cons(eval(E1, env), env))
  eval({CFun E},env)      = <closure {fun {} E}, env>
  eval({CCall E1 E2},env1)    = eval(B,cons(eval(E2, env), env2))
                              if eval(E1,env) = <closure {fun {} B}, env2>
                             = error!  otherwise
  eval(CRef(N), env)         = list-ref(env, N)
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Fun  (Listof Symbol) BRANG]
  [Call BRANG (Listof BRANG)])

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef   Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> BRANG)
;; parses s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (: parse-sexprs : (Listof Sexpr) -> (Listof BRANG))
  ;; utility for parsing a list of expressions
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: names) ...) body)
        (Fun names (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg ...)
     (Call (parse-sexpr fun) (parse-sexprs arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(define-type ENV = (Listof VAL))

(define-type DE-ENV = Symbol -> Natural)

(: de-empty-env : Symbol -> Natural)
;; Empty environment mapping to bootstrap creation of de-env
;; If it is called it will throw an error
(define (de-empty-env sym)
  (error 'de-empty-env "Empty env does not have any mappings for ~s" sym))

(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

(: de-extend : Symbol DE-ENV -> DE-ENV)
;; Extend the environment by mapping symbol 'sym' to 0.
;; Add 1 and recurse if the symbol is not present
;; If the element doesn't exist, the first function will be called,
;; i.e de-empty-env
(define (de-extend sym env)
  (lambda (x)
    (if (eq? x sym)
        0
        (add1 (env x)))))

(: preprocess-fun : BRANG Symbol (Listof Symbol) DE-ENV -> CORE)
;; Curry all args into CFuns
(define (preprocess-fun body first-arg args env)
  (let ([next-env (de-extend first-arg env)])
    (if (null? args)
        (CFun (preprocess body next-env))
        (CFun (preprocess-fun body (first args) (rest args) next-env)))))

(: preprocess-call : BRANG BRANG (Listof BRANG) DE-ENV -> CORE)
;; Call curried CFuns by folding the args into CCalls
(define (preprocess-call fun first-arg args env)
  (: fold-calls : BRANG CORE -> CORE)
  ;; Helper function to be used while folding BRANGs to CORE
  (define (fold-calls arg acc-calls) (CCall acc-calls (preprocess arg env)))
  (let ([fun (preprocess fun env)] ; Ensures functions and args are processed
        [first-arg (preprocess first-arg env)])
    (foldl fold-calls
           (CCall fun first-arg)
           args)))

(: preprocess : BRANG DE-ENV -> CORE)
;; Converts BRANG to CORE representation using de bruijn indices
(define (preprocess brexpr env)
  (cases brexpr
    [(Num n)       (CNum n)]
    [(Add lhs rhs) (CAdd (preprocess lhs env) (preprocess rhs env))]
    [(Sub lhs rhs) (CSub (preprocess lhs env) (preprocess rhs env))]
    [(Mul lhs rhs) (CMul (preprocess lhs env) (preprocess rhs env))]
    [(Div lhs rhs) (CDiv (preprocess lhs env) (preprocess rhs env))]
    [(Id name)     (CRef (env name))]
    [(With name expr body) ; expr is the arg and body is the fun body
     (CCall (CFun (preprocess body (de-extend name env)))
            (preprocess expr env))]
    [(Fun names body) (preprocess-fun body (first names) (rest names) env)]
    [(Call fun args) (preprocess-call fun (first args) (rest args) env)]))

(: NumV->number : VAL -> Number)
;; convert a BRANG runtime numeric value to a Racket one
(define (NumV->number val)
  (cases val
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" val)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : CORE ENV -> VAL)
;; evaluates CORE expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef n) (list-ref env n)]
    [(CFun bound-body)
     (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (define fval (eval fun-expr env))
     (cases fval
       [(FunV bound-body f-env)
        (eval bound-body
              (cons (eval arg-expr env) f-env))]
       [else (error 'eval "`call' expects a function, got: ~s"
                    fval)])]))

(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) de-empty-env) '())])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{with {f {with {x 3} {fun {y} {+ x y}}}}
              {with {x 100}
                {call f 4}}}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)

(test (run "{call {fun {x y z} {+ {+ x y} z}} 1 2 3}") => 6)
(test (run "{+ 1 2}") => 3)
(test (run "{+ 1 h}") =error>
      "de-empty-env: Empty env does not have any mappings for h")
(test (run "{/ 15 3}") => 5)
(test (run "{* 1 2}") => 2)
(test (run "{- 1 2}") => -1)
(test (run "{fun {x} x}") =error>
      "run: evaluation returned a non-number: (FunV (CRef 0) ())")
(test (run "{with {+ x 1}}") =error>
      "parse-sexpr: bad `with' syntax in (with (+ x 1))")
(test (run "{call {fun {+ x y}} 1 2}") =error>
      "parse-sexpr: bad `fun' syntax in (fun (+ x y))")
(test (run "{wrong-expr {}}") =error>
      "parse-sexpr: bad syntax in (wrong-expr ())")
(test (run "{call 5 3}") =error>
      "eval: `call' expects a function, got: (NumV 5)")
(test (run "{+ {fun {x} x} 1}") =error>
      "arith-op: expected a number, got: (FunV (CRef 0) ())")
(test (run "{+ {fun {x} x} 1}") =error>
      "arith-op: expected a number, got: (FunV (CRef 0) ())")

(define minutes-spent 300)