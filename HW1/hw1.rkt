(define I-will-behave #t)

;; Question 4
;; B B B B -> Number
;; B must be 0 or 1; returns -1 if any of the arguments are bad
;; Convert 4 digit binary numbers to integers

(define (bin4-to-num n1 n2 n3 n4)
  (if (and (or (= n1 0) (= n1 1))
           (or (= n2 0) (= n2 1))
           (or (= n3 0) (= n3 1))
           (or (= n4 0) (= n4 1)))
      (+ n1 (* n2 2) (* n3 4) (* n4 8))
      -1))

;; Test cases
(equal?  13 (bin4-to-num 1 0 1 1))
(not (equal? 11 (bin4-to-num 1 0 1 1)))
(equal?  8 (bin4-to-num 0 0 0 1))
(equal? 0 (bin4-to-num 0 0 0 0))
(equal? -1 (bin4-to-num 2 0 0 0))
(equal? -1 (bin4-to-num 0 2 0 0))
(equal? -1 (bin4-to-num 0 0 2 0))
(equal? -1 (bin4-to-num 0 0 0 2))

;; Question 5
;; N N -> Integer
;; N: Non-negative Integer
;; Returns the greatest common divisor (GCD) of a and b using the binary
;; GCD algorithm.
(define (gcd2 a b)
  (cond
    [(= a 0) b]
    [(= b 0) a]
    [(and (even? a) (even? b)) (* 2 (gcd2 (/ a 2) (/ b 2)))]
    [(and (even? a) (odd? b)) (gcd2 (/ a 2) b)]
    [(and (odd? a) (even? b)) (gcd2 a (/ b 2))]
    [(and (odd? a) (odd? b) (>= a b)) (gcd2 (/ (- a b) 2) b)]
    [(and (odd? a) (odd? b) (< a b)) (gcd2 (/ (- b a) 2) a)]))

;; Test cases
(equal? (gcd2 0 7) 7) ; a = 0
(equal? (gcd2 8 0) 8) ; b = 0

(equal? (gcd2 378 144) 18)  ; a and b even
(equal? (gcd2 216 612) 36) ; a and b even

(equal? (gcd2 10 15) 5) ; a is even b is odd
(equal? (gcd2 216 33) 3) ; a is even b is odd

(equal? (gcd2 77 22) 11) ; a is odd b is even
(equal? (gcd2 999 300) 3) ; a is odd b is even

(equal? (gcd2 777 77) 7) ; a and b odd a > b
(equal? (gcd2 9999 99) 99) ; a and b odd a > b

(equal? (gcd2 77 777) 7) ; a and b odd b > a
(equal? (gcd2 99 9999) 99) ; a and b odd b > a

(equal? (gcd2 0 0) 0)      ; Both a and b are 0
(equal? (gcd2 1 1) 1)      ; Both a and b are 1
(equal? (gcd2 2 4) 2)      ; One divides the other
(equal? (gcd2 1000000 1) 1) ; Large and small

(equal? (gcd2 101 103) 1)  ; Both are primes
(equal? (gcd2 14 15) 1)    ; Co-prime numbers

;; Question 6
;; List[Number] -> Boolean
;; Returns true if all the numbers in the list are even, false otherwise.
(define (all-even? li)
  (cond [(empty? li) #t]
        [else (and (even? (first li)) (all-even? (rest li)))]))

;; Test cases
(all-even? '())
(all-even? '(4 2 6))
(not (all-even? '(1 2 3)))
(not (all-even? '(2 4 6 8 9 10)))
(all-even? '(20 4000 3000 222 0 111112))

;; Question 7
;; List[Number], List[Number] -> List[Number]
;; Merge two lists and return one sorted list.
(define (merge-lists list_a list_b)
  (cond [(empty? list_a) list_b]
        [(empty? list_b) list_a]
        [else (if (< (first list_a) (first list_b))
                  (cons (first list_a) (merge-lists (rest list_a) list_b))
                  (cons (first list_b) (merge-lists list_a (rest list_b))))]))


;; Test cases
(equal? '(1 2 3 4 5 6) (merge-lists '(1 3 5) '(2 4 6)))
(equal? '(1 2 3 4 5 6) (merge-lists '(2 4 6) '(1 3 5)))
(equal? '(1 2 3 4) (merge-lists '(1 3) '(2 4)))
(equal? '(1 2 3 4 5 6) (merge-lists '(1 3) '(2 4 5 6)))
(equal? '(2 4 5 6) (merge-lists '() '(2 4 5 6)))
(equal? '(2 4 5 6) (merge-lists '(2 4 5 6) '()))
(equal? '() (merge-lists '() '()))

(define minutes-spent 240)
