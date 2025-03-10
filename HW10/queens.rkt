#lang pl 10

#|
  Skeleton code for the queens problem.

  Reminder: in Schlac all values are one-argument functions, so you'll
  never get any kind of a "type error". You can only get such errors
  when you use one of the special conversion-to-racket with a bad value
  if you're lucky -- and a meaningless result if you're not. This means
  that you need to be extremely careful about the types: they're only
  comments, but make sure that you use the right types. Use the test
  cases as much as possible to make your life easier. If you get stuck,
  you can try to implement the problem in the course language, then
  translate the Racket code back to Schlac. See the homework text for
  details.
|#

;; a convenient rewrite for local bindings
(rewrite (with [x E1] E2) => ((lambda (x) E2) E1))
;; tests
(test (->nat (with [x (* 2 4)] (+ x x))) => '16)
(test (->nat (with [x 3] (with [x (* x 3)] (+ x x)))) => '18)

;; ==================== Numeric utilities ====================

;; diff : Nat Nat -> Nat
;; computes the difference between two numbers: |x-y|
(define diff
  (lambda (x y)
    (+ (- x y)(- y x))))
;; tests
(test (->nat (diff 0 0)) => '0)
(test (->nat (diff 3 3)) => '0)
(test (->nat (diff 2 4)) => '2)
(test (->nat (diff 4 2)) => '2)

;; = : Nat Nat -> Bool
;; comparison for natural numbers
(define =
  (lambda (x y)
    (zero? (diff x y))))
;; tests
(test (->bool (= 0 0)) => '#t)
(test (->bool (= 1 1)) => '#t)
(test (->bool (= 2 1)) => '#f)
(test (->bool (= (* (* 2 3) 2) (* 3 4))) => '#t)

;; ;; ==================== List utilities ====================

;; append : (Listof A) (Listof A) -> (Listof A)
;; appends two lists
(define/rec append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2)))))

;; tests
(test (->listof ->nat (append null null)) => '())
(test (->listof ->nat (append null l123)) => '(1 2 3))
(test (->listof ->nat (append l123 null)) => '(1 2 3))
(test (->listof ->nat (append l123 l123)) => '(1 2 3 1 2 3))

;; ;; append* : (Listof (Listof A)) -> (Listof A)
;; ;; appends a list of lists
(define/rec append*
  (lambda (lists)
    (if (null? lists)
        null
        (append (car lists) (append* (cdr lists))))))
;; tests
(test (->listof ->nat (append* null)) => '())
(test (->listof ->nat (append* (cons null null))) => '())
(test (->listof ->nat (append* (cons l123 (cons l123 null))))
      => '(1 2 3 1 2 3))

;; ;; map : (A -> B) (Listof A) -> (Listof B)
;; ;; maps a function over a list, returning a list of result values
(define/rec map
  (lambda (fun list)
    (if (null? list) null
        (cons (fun (car list)) (map fun (cdr list))))))
;; tests
(test (->listof ->nat (map add1 null)) => '())
(test (->listof ->nat (map (+ 2) l123)) => '(3 4 5))
;; the `map' examples below are just for fun
(test (->listof ->bool
                (with [tf (cons #t (cons #f null))]
                      (append* (map (lambda (x) (map (and x) tf)) tf))))
      => '(#t #f #f #f))
;; this is doing the same, using the fact that `not' returns a
;; function with swapped arguments
(test (->listof ->bool
                (with [tf (cons #t (cons #f null))]
                      (append* (map (not map tf) (map and tf)))))
      => '(#t #f #f #f))
;; (map and tf) = (not map tf and), so abstract the (not map tf) part
(test (->listof ->bool
                (with [nmtf (not map (cons #t (cons #f null)))]
                      (append* (map nmtf (nmtf and)))))
      => '(#t #f #f #f))
;; finally, do this for both `and' and `or'
(test (->listof ->bool
                (with [nmtf (not map (cons #t (cons #f null)))]
                      (with [ao (cons and (cons or null))]
                            (append* (map nmtf (append* (map nmtf ao)))))))
      => '(#t #f #f #f #t #t #t #f))

;; filter : (A -> Bool) (Listof A) -> (Listof A)
;; filters a list according to a given predicate
(define/rec filter
  (lambda (pred list)
    (if (null? list)
        null
        (with [first (car list)]
              (with [rest (cdr list)]
                    (if (pred first)
                        (cons first (filter pred rest))
                        (filter pred rest)))))))
;; tests
(define =3 (= 3))
(test (->listof ->nat (filter =3 null)) => '())
(test (->listof ->nat (filter =3 l123)) => '(3))
(test (->listof ->nat (filter (lambda (x) (= 1 (diff x 2))) l123))
      => '(1 3))
;; Define test lists
(define l12345 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 null))))))
(define l1to5 l12345)

;; Test 1: Filter even numbers (keep 2, 4)
(test (->listof ->nat (filter (lambda (x) (= 0 (mod x 2))) l1to5)) => '(2 4))

;; Test 2: Filter numbers greater than 3 (keep 4, 5)
(test (->listof ->nat (filter (lambda (x) (> x 3)) l1to5)) => '(4 5))

;; Test 3: Filter numbers equal to 5 (keep 5)
(test (->listof ->nat (filter (lambda (x) (= x 5)) l1to5)) => '(5))

;; Test 4: Filter numbers less than 2 (keep 1)
(test (->listof ->nat (filter (lambda (x) (< x 2)) l1to5)) => '(1))

;; Test 5: Filter with always-true predicate (keep all)
(test (->listof ->nat (filter (lambda (x) #t) l1to5)) => '(1 2 3 4 5))

;; Test 6: Filter with always-false predicate (keep none)
(test (->listof ->nat (filter (lambda (x) #f) l1to5)) => '())

;; Test 7: Filter odd numbers (keep 1, 3, 5)
(test (->listof ->nat (filter (lambda (x) (= 1 (mod x 2))) l1to5)) => '(1 3 5))

;; Test 8: Filter numbers not equal to 2 (keep 1, 3, 4, 5)
(test (->listof ->nat (filter (lambda (x) (not (= x 2))) l1to5)) => '(1 3 4 5))

;; ==================== Main code ====================

;; From this point we start with the actual solution. A configuration of
;; a board is a list of numbers: each number represents the column of a
;; queen at that row. For example (0 1 0) represents three rows, where
;; the queen of the first row stands on the first column, the second row
;; queen is at the second column, and the third is on the first column
;; too.

;; threaten? : Nat Nat Nat -> Bool
;; determines whether a queen at column x and a queen at column y are
;; threatening each other when they are n rows apart
(define threaten?
  (lambda (x y n)
    (or (or (zero? n) (= x y)) (= (diff x y) n))))
;; tests
;; if both are at column 1 (ie, the second column), then they always
;; threaten each other
(test (->bool (threaten? 1 1 1)) => '#t)
(test (->bool (threaten? 1 1 2)) => '#t)
(test (->bool (threaten? 1 1 3)) => '#t)
;; if the first is at 1 and the second at 3, then they threaten each
;; other if they are two rows apart
(test (->bool (threaten? 1 3 1)) => '#f)
(test (->bool (threaten? 1 3 2)) => '#t)
(test (->bool (threaten? 1 3 3)) => '#f)
;; same when the columns are swapped
(test (->bool (threaten? 3 1 1)) => '#f)
(test (->bool (threaten? 3 1 2)) => '#t)
(test (->bool (threaten? 3 1 3)) => '#f)
;; Test same column with n=0 (always threatens)
(test (->bool (threaten? 2 2 0)) => '#t)
(test (->bool (threaten? 5 5 0)) => '#t)

;; Test different columns with n=0 (always threatens)
(test (->bool (threaten? 2 5 0)) => '#t)
(test (->bool (threaten? 0 3 0)) => '#t)

;; Test diagonal threats (|x - y| = n)
(test (->bool (threaten? 2 5 3)) => '#t)   
(test (->bool (threaten? 5 2 3)) => '#t)   
(test (->bool (threaten? 0 3 3)) => '#t)   

;; Test non-diagonal threats (|x - y| ≠ n)
(test (->bool (threaten? 2 5 2)) => '#f)   
(test (->bool (threaten? 1 4 2)) => '#f)   

;; Test edge cases with minimal values
(test (->bool (threaten? 0 0 0)) => '#t)   
(test (->bool (threaten? 0 1 1)) => '#t)   

;; Correct test:
(test (->bool (threaten? 0 1 1)) => '#t)   
(test (->bool (threaten? 1 0 1)) => '#t)   

;; safe? : Nat Nat (Listof Nat) -> Bool
;; determines whether it's safe to put a queen at column col when it's n
;; rows before a list of column positions cols; in other words, (safe
;; col 1 cols) determines if you can put a queen at column col just
;; before the configuration specified by cols; (this is a helper for
;; `configurations' below, note that the cols argument is essentially a
;; part of a whole-board configuration)
(define/rec safe?
  (lambda (col n cols) ;; (1 1 '(2 4))
    (or (null? cols)
        (and (not (threaten? col (car cols) n))
             (safe? col (add1 n) (cdr cols))))))
;; tests
(test (->bool (safe? 5 1 l123)) => '#t)
(test (->bool (safe? 5 3 l123)) => '#t)
(test (->bool (safe? 5 2 l123)) => '#f)
(test (->bool (safe? 0 1 l123)) => '#f)
(test (->bool (safe? 1 0 l123)) => '#f)
(test (->bool (safe? 3 0 l123)) => '#f)
(test (->bool (safe? 1 1 l123)) => '#f)
(test (->bool (safe? 2 1 l123)) => '#f)
(test (->bool (safe? 3 1 l123)) => '#f)
(test (->bool (safe? 4 1 l123)) => '#f)

;; configurations : Nat (Listof Nat) -> (Listof (Listof Nat))
;; this is the core of the solution: finds all valid n-row
;; configurations where the queen columns are all taken from the given
;; list of input cols
(define/rec configurations
  (lambda (n cols)
    (if (zero? n)
        (cons null null)
        (append* (map (lambda (rest)
                        (map (lambda (col) (cons col rest))  
                             (filter (lambda (col) (safe? col 1 rest))
                                     cols)))
                      (configurations (sub1 n) cols))))))

;; No need to test this, since the main `queens' function is a simple
;; call to this function. But to clarify, if you call it with
;; (configurations 2 (list 1 2 4)), then the result will be a list of
;; "board configurations" of queen placements: each such configuration
;; is a list of 2 column numbers from the (list 1 2 4), such that no
;; queen threatens another queen. In this case, (list 2 4) is a valid
;; result, (list 1) is invalid (need two rows not one), and (list 1 2)
;; is not (because it represents two diagonally placed queens).
;;
;; range : Nat Nat -> (Listof Nat)
;; returns the list of numbers starting from the first argument, up to
;; (but not including) the second argument
(define/rec range
  (lambda (from to)
    (if (= to from) null (cons from (range (add1 from) to)))))
;; tests
(test (->listof ->nat (range 0 0)) => '())
(test (->listof ->nat (range 0 5)) => '(0 1 2 3 4))
(test (->listof ->nat (range 2 5)) => '(2 3 4))

;;
;; queens : Nat -> (Listof Nat)
;; finally, the solution is simple -- find all the configurations of
;; `size' rows, where each row has a queen at some column 0 to `size'
;; and return the first one, or return an empty list if there are no
;; solutions
(define queens
  (lambda (size)
    (with [solutions (configurations size (range 0 size))]
      (if (null? solutions)
          null
          (car solutions)))))
;; tests
;; single solution for a 1x1 board:
(test (->listof ->nat (queens 1)) => '(0))
;; no solution for 2x2 or 3x3 boards
(test (->listof ->nat (queens 2)) => '())
(test (->listof ->nat (queens 3)) => '())
;; and finally test a few solution (note that these tests depend on
;; the specific algorithm since there are many correct solutions, so
;; they are *not* good tests)
(test (->listof ->nat (queens 4)) => '(2 0 3 1))
(test (->listof ->nat (queens 5)) => '(3 1 4 2 0))

;; 8 : Nat
;; define this so you can run the real problem conveniently
(define 8 (+ 4 4))
;; tests
(test (->nat 8) => '8)

;; Finally, this is what you want to try:
;;   (->listof ->nat (queens 8))
(test (->listof ->nat (queens 8)) => '(3 1 6 2 5 7 4 0))
;; You can also try to see how long it takes to find all solutions by
;; making `queens' return the whole list, and use it like this:
;;   (->listof (->listof ->nat) (queens-all 4))
(define hours-spent 4)
