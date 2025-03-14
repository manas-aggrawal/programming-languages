#lang pl 11

(define (Y f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (z) ((x x) z))))))

(rewrite (define/rec (f x ...) E)
         => (define f
              (let ([g (Y (lambda (f)
                            (lambda (_)
                              (lambda (x ...)
                                (let ([f (f #f)]) E)))))])
                (g #f))))

;; ( : ackermann : Number Number -> Number)
;; Recursive implementation of ackermann function with two arguments.
(define/rec (ackermann m n)
  (cond [(zero? m) (+ n 1)]
        [(zero? n) (ackermann (- m 1) 1)]
        [else      (ackermann (- m 1) (ackermann m (- n 1)))]))
(test (ackermann 3 3) => 61)

(rewrite (letfuns ([(f x ...) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda(_)
                            (lambda (name)
                              (match name
                                ['f
                                 (lambda (x ...)
                                     (let ([f ((funs #f) 'f)] ...) E))]
                                ...)))))])
              (let ([f ((g #f) 'f)] ...) B)))


(letfuns ([(even? n) (if (= n 0) #t (odd?  (- n 1)))]
          [(odd?  n) (if (= n 0) #f (even? (- n 1)))])
  (even? 0))
;; TODO test even and odd functions

;; an extended example
(define scan
  (letfuns ([(start str)  (loop (explode-string str) 0)]
            [(loop l n)   (match l
                            [(list)
                             (zero? n)]
                            [(cons 'open more)
                             (loop more (add1 n))]
                            [(cons 'close more)
                             (and (> n 0) (loop more (sub1 n)))]
                            [(cons (number: m) more)
                             (nums more m n)]
                            [(cons _ more)
                             (loop more n)])]
            [(nums l m n) (match l
                            [(cons (number: m1) more)
                             (and (< m m1) (nums more m1 n))]
                            [else (loop l n)])])
    start))
(test (scan "(()123(246x12)) (blah)"))
(test (not (scan "(1232)")))
(test (not (scan "()(")))
(test (not (scan "())")))
