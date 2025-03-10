#lang pl 07

;; Q1
;; Generally, stack frames store information about the current execution
;; context so it can be restored when a called function returns. However,
;; in this system, the `goto` statement is never expected to return, meaning
;; that preserving stack frames would lead to infinite stack growth.
;; Essentially, Tail call optimization is needed in this case, as it ensures
;; no unnecessary stack frames are saved when jumping to new thunks,
;; preventing memory leaks.

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

;; "X -= Y"
;; decrements register X by register Y
(: sub : Register Register -> Void)
(define (sub X Y) (set-box! X (- (unbox X) (unbox Y))))

;; "X -= N"
;; decrements register X by a constant N
(: subi : Register Integer -> Void)
(define (subi X N) (set-box! X (- (unbox X) N)))

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
(define (more-ones? a b)
  (: A : Register) (define A (box a))
  (: B : Register) (define B (box b))
  (: TotalA : Register) (define TotalA (box 0))
  (: TotalB : Register) (define TotalB (box 0))
  (: Temp : Register) (define Temp (box 0))
  (: Target : Register) (define Target (box 0))
  (: TotalTarget : Register) (define TotalTarget (box 0))
  (: CountedB : Register) (define CountedB (box 0))
  (: R : Register) (define R (box 0))
  ;;
  (: main : Label)
  (: sumA : Label)
  (: step : Label)
  (: next : Label)
  (: sumB : Label)
  (: count : Label)
  (: compare : Label)
  (: setTrue : Label)
  ;;
  (define (main) (goto sumA))
  ;;
  (define (sumA)
    (mov Target A)
    (goto count))
  ;;
  (define (count)
    (if0 Target next step))
  (define (step)
    (mov Temp Target)
    (andi Temp 1)
    (add TotalTarget Temp) ; if last digit was 1, Temp is 1 otherwise it is 0
    (shri Target 1) ; Get rid of last bit
    (goto count))
  ;;
  (define (next)
    (ifp CountedB compare sumB))
  ;;
  (define (sumB)
    (mov TotalA TotalTarget)
    (movi TotalTarget 0)
    (mov Target B)
    (movi CountedB 1)
    (goto count))
  ;;
  (define (compare)
    (mov TotalB TotalTarget)
    (sub TotalA TotalB)
    (ifp TotalA setTrue halt))
  (define (setTrue)
    (movi R 1)
    (halt))
  ;;
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
(test (more-ones? 400 401) => 0)
(test (more-ones? 500 400) => 1)
(test (more-ones? 3 8192) => 1)

(define minutes-spent 110)
