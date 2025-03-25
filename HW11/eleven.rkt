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
(test (ackermann 0 0) => 1)  
(test (ackermann 0 5) => 6)  
(test (ackermann 1 0) => 2)  
(test (ackermann 1 1) => 3)
(test (ackermann 2 2) => 7)
(test (ackermann 2 5) => 13)
(test (ackermann 3 0) => 5)  

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
         (and (even? 0)       
              (not (even? 1)) 
              (odd? 1)        
              (not (odd? 0))))

(test (letfuns ([(even? n) (if (= n 0) #t (odd?  (- n 1)))]
                [(odd?  n) (if (= n 0) #f (even? (- n 1)))])
               (list (even? 0)       
                     (even? 2)       
                     (even? 4)       
                     (odd? 1)        
                     (odd? 3)        
                     (even? 1)       
                     (odd? 0)        
                     (odd? 2)))      
      => '(#t #t #t #t #t #f #f #f))

;; (: scan : String -> Boolean)
;; scan validates if in an input string:
;; 1. Parentheses (represented by tokens 'open and 'close)
;;    are properly nested/balanced
;; 2. Number sequences (e.g., "123", "246") appear in strictly increasing order
;;
;; Non-parenthesis or non-number characters (like letters)
;; are ignored during validation.
;; Returns #t if the string satisfies both conditions, #f otherwise.
;;
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
(test (scan "") => #t)                   
(test (scan "()") => #t)                 
(test (scan "(open)") => #t)             
(test (scan "123") => #t)                
(test (scan "123 456") => #t)            
(test (scan "((()))") => #t)             
(test (scan "(open (open close) close)") => #t)
(test (not (scan "(")))                  
(test (not (scan ")")))                  
(test (not (scan "123 123")) => #f)            
(test (not (scan "123 122")))            
(test (not (scan "openclose")) => #f)          
(test (not (scan "(open close")))        
(test (not (scan "123x")) => #f)               
(test (not (scan "())(")))               
(test (not (scan "(close open)")) => #f)
(test (scan "1 2 3 4 5") => #t)         
(test (scan "5 6 7") => #t)
(test (not (scan "5 5 6")) => #f)       
(test (not (scan "6 5")) => #f)         
(test (scan "(()123(246x12)) (blah)") => #t)
(test (not (scan "(()123(246 122))")))      

(define minutes-spent 120)