#lang pl 07

;; Tail call elimination preserves the recursive stack of the program
;; which would otherwise refresh if we were to use Tail call. Also,
;; it maintains mutable box values between iterations.
;; In the given test case example let's consider the case of finding
;; 3rd num in fibonacci series. During first iteration it will add
;; A and B and put it in C and transfer B to A and C to B and
;; decrement N and then goto again calls main.
;; Now, if we had used tail recursion then 2nd call to main func
;; would've created a new stack and forgotten that we decremented N
;; and changed values of A, B and C to avoid this and optimise this
;; we do tail call elimination where instead of calling func recursively
;; we use goto jump statement.


;; we represent labels (goto targets) as void thunks, and registers (or
;; memory locations in general) as integer boxes.
(define-type Label    = (-> Void))
(define-type Register = (Boxof Integer))

;; "X = Y"
;; assigns the contents of register Y to register X
(: mov : Register Register -> Void)
(define (mov X Y) (set-box! X (unbox Y)))

;; "X = N"
;; assigns the constant N (an "immediate" value)  to register X
(: movi : Register Integer -> Void)
(define (movi X N) (set-box! X N))

;; "X += Y"
;; increments register X by register Y
(: add : Register Register -> Void)
(define (add X Y) (set-box! X (+ (unbox X) (unbox Y))))

;; "X += N"
;; increments register X by a constant N
(: addi : Register Integer -> Void)
(define (addi X N) (set-box! X (+ (unbox X) N)))

;; "X -= Y"
;; decrements register X by register Y
(: sub : Register Register -> Void)
(define (sub X Y) (set-box! X (- (unbox X) (unbox Y))))

;; "X -= N"
;; decrements register X by a constant N
(: subi : Register Integer -> Void)
(define (subi X N) (set-box! X (- (unbox X) N)))

;; "X &= Y"
;; sets X to the bitwise "and" of X and Y
(: and : Register Register -> Void)
(define (and X Y) (set-box! X (bitwise-and (unbox X) (unbox Y))))

;; "X &= N"
;; sets X to the bitwise "and" of X and a constant N
(: andi : Register Integer -> Void)
(define (andi X N) (set-box! X (bitwise-and (unbox X) N)))

;; "X >>= N"
;; shifts register X right by N bits
(: shri : Register Integer -> Void)
(define (shri X N) (set-box! X (arithmetic-shift (unbox X) (- N))))

;; "goto L"
;; (goto L) jumps to the label -- labels are represented as nullary
;; functions (also called "thunks")
(: goto : Label -> Void)
(define (goto L) (L))

;; "halt"
;; halt execution, same as `void` (which is the trivial Racket function
;; that returns a Void value)
(: halt : -> Void)
(define halt void)

;; "if X=0 goto L1 else goto L2"
;; if register X is zero, jump to L1, else jump to L2
(: if0 : Register Label Label -> Void)
(define (if0 a l1 l2) (if (zero? (unbox a)) (goto l1) (goto l2)))

;; "if X>0 goto L1 else goto L2"
;; if register X is positive, jump to L1, else jump to L2
(: ifp : Register Label Label -> Void)
(define (ifp a l1 l2) (if (positive? (unbox a)) (goto l1) (goto l2)))

(: fib : Integer -> Integer)
;; compute the nth fibonacci number using the assembly language
(define (fib n)
  (: A : Register) (define A (box 0))
  (: B : Register) (define B (box 1))
  (: C : Register) (define C (box 0))
  (: N : Register) (define N (box n))
  ;;
  (: main : Label)
  (: step : Label)
  ;;
  (define (main) (if0  N halt step))
  (define (step) (mov  C A)
    (add  C B)
    (mov  A B)
    (mov  B C)
    (subi N 1)
    (goto main))
  ;;
  (main)
  (unbox A))

;; test
(test (map fib '(0 1 2 3 4 5 6 7 8 9 10))
      => '(0 1 1 2 3 5 8 13 21 34 55))

(: more-ones? : Integer Integer -> Integer)
;; returns 1 if `a' has more 1s in its binary representation than `b'
(: more-ones? : Integer Integer -> Integer)
(define (more-ones? a b)
  ;; Input Registers
  (: A : Register) (define A (box a))
  (: B : Register) (define B (box b))
  
  ;; Counting Registers
  (: CountA : Register) (define CountA (box 0))
  (: CountB : Register) (define CountB (box 0))
  (: Current : Register) (define Current (box 0))
  (: Temp : Register) (define Temp (box 0))
  
  ;; Control Registers
  (: Phase : Register) (define Phase (box 1))
  
  ;; Result Register
  (: R : Register) (define R (box 0))
  
  ;; Control Flow Labels
  (: main : Label)
  (: count_loop : Label)
  (: shift_right : Label)
  (: count_done : Label)
  (: compare : Label)
  (: set_r_1 : Label)
  (: end : Label)
  
  (define (main)
    (mov Current A)
    (movi CountA 0)
    (movi Phase 1)
    (goto count_loop))

  (define (count_loop)
    (if0 Current count_done
         (lambda ()
           (mov Temp Current)
           (andi Temp 1)
           (if0 Temp (lambda () (goto shift_right))
                (lambda ()
                  (if0 Phase
                       (lambda () (addi CountB 1) (goto shift_right))
                       (lambda () (addi CountA 1) (goto shift_right))))))))

  (define (shift_right)
    (shri Current 1)
    (goto count_loop))

  (define (count_done)
    (if0 Phase
         compare
         (lambda ()
           (mov Current B)
           (movi CountB 0)
           (movi Phase 0)
           (goto count_loop))))

  (define (compare)
    (mov Temp CountA)
    (sub Temp CountB)
    (ifp Temp set_r_1 (lambda () (goto end))))

  (define (set_r_1)
    (movi R 1)
    (goto end))

  (define (end)
    (halt))

  (main)
  (unbox R))

;; tests
(test (more-ones? 0 0) => 0)
(test (more-ones? 1 0) => 1)
(test (more-ones? 1 2) => 0)
(test (more-ones? 2 0) => 1)
(test (more-ones? 0 1) => 0)
(test (more-ones? 0 2) => 0)
(test (more-ones? 2 1) => 0)
(test (more-ones? 2 2) => 0)
(test (more-ones? 3 1) => 1)
(test (more-ones? 5 3) => 0)   
(test (more-ones? 15 7) => 1)  
(test (let ()
        (: X : Register) (define X (box #b1100)) ; 12
        (: Y : Register) (define Y (box #b1010)) ; 10
        (and X Y) 
        (unbox X))
      => 8)

(define minutes-spent 300)
