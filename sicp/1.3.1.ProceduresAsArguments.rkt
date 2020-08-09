#lang sicp
(#%require (lib "27.ss" "srfi"))

#| Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration
than the method illustrated above. Using Simpson's Rule, the integral of a function
 f between a and b is approximated as

where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n
increases the accuracy of the approximation.) Define a procedure that takes as
arguments f, a, b, and n and returns the value of the integral, computed using
Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n =
100 and n = 1000), and compare the results to those of the integral procedure
shown above. |#

(define (inc n) (+ n 1))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (integral-simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (yk-term k)
      (let ((yk (f (+ a (* k h)))))
      (cond ((or (= k 0) (= k n)) yk)
            ((even? k) (* 2 yk))
            (else (* 4 yk)))))
    (* (/ h 3) (sum yk-term 0 inc n)))
)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))
; (integral cube 0 1 0.01)          ; 0.24998750000000042
; (integral-simpson cube 0 1 100)   ; 1/4
; (integral cube 0 1 0.001)         ; 0.249999875000001
; (integral-simpson cube 0 1 1000)  ; 1/4


;; Exercise 1.30.
#| The sum procedure above generates a linear recursion. The procedure can be
rewritten so that the sum is performed iteratively. Show how to do this by
filling in the missing expressions in the following definition: |#

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31.
#| a.  The sum procedure is only the simplest of a vast number of similar abstractions
that can be captured as higher-order procedures.51 Write an analogous procedure
called product that returns the product of the values of a function at points over
a given range. Show how to define factorial in terms of product. Also use product
to compute approximations to  using the formula |#

(define (prod term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod term (next a) next b))))

(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (id x) x)

(define (fact n)
  (prod id 1 inc n))

 (define (wallis-term n)
  (* (/ (* 2.0 n)
        (- (* 2.0 n) 1.0))
     (/ (* 2.0 n)
        (+ (* 2.0 n) 1.0))))

(define (approx-pi n)
  (* 2 (prod wallis-term 1.0 inc n))
)
; (approx-pi 1000)   ; 3.140807746030383
; (approx-pi 100000) ; 3.14158479965432


;; Exercise 1.32.
#| a. Show that sum and product (exercise 1.31) are both special cases of a still more
general notion called accumulate that combines a collection of terms, using some
general accumulation function:
          (accumulate combiner null-value term a next b)
Write accumulate and show how sum and product can both be defined as simple calls to
accumulate. |#


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b)
)

(define (prod-acc term a next b)
  (accumulate * 1 term a next b)
)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Exercise 1.33.
#| You can obtain an even more general version of accumulate (exercise 1.32) by
introducing the notion of a filter on the terms to be combined. That is, combine
only those terms derived from values in the range that satisfy a specified condition.
The resulting filtered-accumulate abstraction takes the same arguments as accumulate,
together with an additional predicate of one argument that specifies the filter.
Write filtered-accumulate as a procedure. |#


(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))
  )
)

#| Show how to express the following using filtered-accumulate:
a. the sum of the squares of the prime numbers in the interval a to b (assuming
that you have a prime? predicate already written) |#

(define (square x) (* x x))
(define (fast-prime? n times)
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (random-integer n)))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                    m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                    m))))
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (sum-square-primes a b)
  (filtered-accumulate (lambda (x) (fast-prime? x (* 2 x)))
                        + 0 square a inc b)
)

#| b. the product of all the positive integers less than n that are relatively
prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1). |#

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(define (sum-rel-prime n)
  (filtered-accumulate
        (lambda (x) (= (gcd n x) 1))
        + 0 id 1 inc (- n 1)))


;; Exercise 1.34.
#| Suppose we define the procedure
    (define (f g)
      (g 2))
Then we have
(f square) 4
(f (lambda (z) (* z (+ z 1)))) 6
What happens if we (perversely) ask the interpreter to evaluate the combination
(f f)? Explain. |#

#|
We will get
(f f)
(f 2)
(2 2)
and the interpreter will shout at us saying (2 2) is not a procedure that takes
arguments. |#


