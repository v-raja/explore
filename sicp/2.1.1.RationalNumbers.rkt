#lang sicp

;; Exercise 2.1.
#| Define a better version of make-rat that handles both positive and negative
arguments. Make-rat should normalize the sign so that if the rational number is
positive, both the numerator and denominator are positive, and if the rational
number is negative, only the numerator is negative. |#
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
        (d (/ d g)))
  (cond ((and (< n 0) (< d 0)) (cons (- n) (- d)))
        ((< d 0) (cons (- n) (- d)))
        (else (cons n d))))))

(make-rat (- 3) 9)

(make-rat (- 2) (- 8))
(make-rat 2 (- 8))


;; Exercise 2.2.
#| Consider the problem of representing line segments in a plane. Each segment is
represented as a pair of points: a starting point and an ending point. Define a
constructor make-segment and selectors start-segment and end-segment that define
the representation of segments in terms of points. Furthermore, a point can be
represented as a pair of numbers: the x coordinate and the y coordinate.
Accordingly, specify a constructor make-point and selectors x-point and y-point
that define this representation. Finally, using your selectors and constructors,
define a procedure midpoint-segment that takes a line segment as argument and
returns its midpoint (the point whose coordinates are the average of the coordinates
of the endpoints). |#

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment pa pb) (cons pa pb))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment s)
    (let ((pa (start-segment s))
          (pb (end-segment s)))
        (cons (average (x-point pa)
                        (x-point pb))
              (average (y-point pa)
                        (y-point pb)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; (print-point (midpoint-segment (make-segment (make-point 2 2) (make-point 4 4)))) ; (3,3)

;; Exercise 2.3.
#| Implement a representation for rectangles in a plane. (Hint: You may want to
make use of exercise 2.2.) In terms of your constructors and selectors, create
procedures that compute the perimeter and the area of a given rectangle. Now
implement a different representation for rectangles. Can you design your system
with suitable abstraction barriers, so that the same perimeter and area procedures
will work using either representation? |#

; The question doesn't mention whether the rectangle should be aligned to the grid
; so we'll challenge ourselves and say it isn't. Now, the 'api' for the rectangle
; should be the same regardless of implementation so we'll define perimeter and
; area first.

(define (peri-rect rect)
    (+ (* 2 (width-rect rect)) (* 2 (height-rect rect))))

(define (area-rect rect)
    (* (width-rect rect) (height-rect rect)))

; Now, our first representation of our rectangle will be with 3 points (since we
; assume the rect can be rotated any which way)

; (define (make-rect p1 p2 p3)
;   (cons p1 (cons p2 p3)))

; The problem with this is that we don't know where p1, p2 and p3 form a legal
; rectangle. So we'll define vectors and dot-product to check if the vectors are
; orthogonal

(define (make-vec p1 p2) (cons (- (x-point p1) (x-point p2))
                                (- (y-point p1) (y-point p2))))
; (define (start-vec v) (car v))
; (define (end-vec v) (cdr v))
; (define (add-vec v1 v2) (make-vec (+ (vec-x v1) (vec-x v2)) (+ (vec-y v1) (vec-y v2))))
(define (dot-prod v1 v2)
      (+ (* (x-point v1) (x-point v2)) (* (y-point v1) (y-point v2))))
(define (orthogonal? v1 v2)
  (= 0.0 (dot-prod v1 v2)))

(define (make-rect p1 p2 p3)
  (cond ((orthogonal? (make-vec p1 p2) (make-vec p2 p3)) (cons p1 (cons p2 p3)))
        ((orthogonal? (make-vec p1 p3) (make-vec p3 p2)) (cons p1 (cons p3 p2)))
        (else (error "Points do not make rectangle"))))

(define (p1-rect rect) (car rect))
(define (p2-rect rect) (car (cdr rect)))
(define (p3-rect rect) (cdr (cdr rect)))

(define (sqrt x)
  (define (iterative-improve good-enuf? improve)
    (define (iter guess)
        (if (good-enuf? guess)
            guess
            (iter (improve guess))))
            (lambda (guess) (iter guess)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define (square x) (* x x))
(define (euc-dist pa pb)
    (sqrt (+ (square (- (x-point pa) (x-point pb)))
    (square (- (y-point pa) (y-point pb))))))

; (define (height-rect rect)
;   (euc-dist (p1-rect rect) (p2-rect rect)))

; (define (width-rect rect)
;   (euc-dist (p2-rect rect) (p3-rect rect)))

; (make-point 0 0)
; (make-rect (make-point 0 0) (make-point 0 3) (make-point 10 3))
; (define rect-a (make-rect (make-point 0 0) (make-point 0 3) (make-point 10 3)))
; (area-rect rect-a) ; 30
; (peri-rect rect-a) ; 26

; The other representaiton of a rectangle can be its origin (bottom left corner),
; its angle with respect to the x-axis, its width and height

(define (rot-vec v theta)
  (let ((cost (cos theta))
        (sint (sin theta))
        (x    (x-point v))
        (y    (y-point v)))
  (make-point (+ (* cost x) (* sint y))
              (+ (* cost y) (* sint x)))))

; (define (make-rect orig theta w h)
;     (cons (cons orig theta) (cons w h)))

(define (width-rect rect) (car (cdr rect)))
(define (height-rect rect) (cdr (cdr rect)))

;; Exercise 2.4.
#| Here is an alternative procedural representation of pairs. For this
representation, verify that (car (cons x y)) yields x for any objects x and y. |#

; (define (cons x y)
;   (lambda (m) (m x y)))

; (define (car z)
;   (z (lambda (p q) p)))
#|
(car z)
(z (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x
|#

; What is the corresponding definition of cdr?
; (define (cdr z)
;   (z (lambda (p q) q)))

;; Exercise 2.5.
#| Show that we can represent pairs of nonnegative integers using only numbers
and arithmetic operations if we represent the pair a and b as the integer that
is the product 2^a 3^b. Give the corresponding definitions of the procedures cons,
car, and cdr. |#

(define (expt a b)
  (define (iter a b prod)
    (cond ((= b 0) prod)
          ((even? b) (iter (square a) (/ b 2) prod))
          (else (iter a (- b 1) (* a prod)))))
  (iter a b 1))

; (expt 2 3)
; (expt 9 2)

(define (my-cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (count-factor num f)
  (define (divides? x y) (= 0 (modulo x y)))
  (define (iter num cnt)
      (if (divides? num f)
          (iter (/ num f) (+ 1 cnt))
          cnt))
  (iter num 0))

(define (my-car z)
    (count-factor z 2))

(define (my-cdr z)
    (count-factor z 3))

(define test (my-cons 19 27))
; (my-car test)
; (my-cdr test)

(define test1 (my-cons 6 9))
; (my-car test1)
; (my-cdr test1)

;; Exercise 2.6.
#| In case representing pairs as procedures wasn't mind-boggling enough, consider
that, in a language that can manipulate procedures, we can get by without numbers
(at least insofar as nonnegative integers are concerned) by implementing 0 and
the operation of adding 1 as |#

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define succ (lambda (x) (+ 1 x)))
(define zero-eval ((zero succ) 0))
(define one-eval (((add-1 zero) succ) 0))
(define two-eval (((add-1 (add-1 zero)) succ) 0))
zero-eval
one-eval
two-eval

#| Define one and two directly (not in terms of zero and add-1). (Hint: Use
substitution to evaluate (add-1 zero)). Give a direct definition of the addition
procedure + (not in terms of repeated application of add-1). |#

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f (((lambda (y) (lambda (z) z)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (z) z) x))))
; (lambda (f) (lambda (x) (f x))) ; one

; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f (((lambda (y) (lambda (z) (y z))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (z) (f z)) x))))
; (lambda (f) (lambda (x) (f (f x)))) ; two

; ; Give a direct definition of the addition procedure + (not in terms of repeated
; ; application of add-1).

; succ: (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))
; a: (lambda (f) (lambda (x) (f (f x)))) {f a times}


(define (plus a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(define one (add-1 zero))
(define two (add-1 one))
(define three (plus one two))
((three succ) 0)

;; Exercise 2.7.
#| Alyssa's program is incomplete because she has not specified the implementation
of the interval abstraction. Here is a definition of the interval constructor: |#
(define (make-interval a b) (cons a b))

; Define selectors upper-bound and lower-bound to complete the implementation.

; (define (upper-bound z)
;   (let ((a (car z))
;         (b (cdr z)))
;     (if (a > b) a b)))

(define upper-bound cdr)

; (define (lower-bound z)
;   (let ((a (car z))
;         (b (cdr z)))
;     (if (a < b) a b)))

(define lower-bound car)
;; Exercise 2.8.
#| Using reasoning analogous to Alyssa's, describe how the difference of two
intervals may be computed. Define a corresponding subtraction procedure, called
sub-interval. |#

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval a b)
  (add-interval a
          (make-interval (- (upper-bound b)) (- (lower-bound b)))))

; (sub-interval (make-interval 6.12 7.48) (make-interval 4.465 4.935))

;; Exercise 2.9.
#| The width of an interval is half of the difference between its upper and lower
bounds. The width is a measure of the uncertainty of the number specified by the
interval. For some arithmetic operations the width of the result of combining two
intervals is a function only of the widths of the argument intervals, whereas for
others the width of the combination is not a function of the widths of the argument
intervals. Show that the width of the sum (or difference) of two intervals is a
function only of the widths of the intervals being added (or subtracted). |#

(define (width z)
  (/ (- (upper-bound z) (lower-bound z)) 2))

(define interval-a (make-interval 1.123 4.32))
(define interval-b (make-interval 2.123 3.32))
(= (+ (width interval-a) (width interval-b)) (width (add-interval interval-a interval-b))) ; #t
(= (+ (width interval-a) (width interval-b)) (width (sub-interval interval-a interval-b))) ; #t

; Give examples to show that this is not true for multiplication or division.
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(= (+ (width interval-a) (width interval-b)) (width (mul-interval interval-a interval-b))) ; #f
(= (+ (width interval-a) (width interval-b)) (width (div-interval interval-a interval-b))) ; #f

;; Exercise 2.10.
#| Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and
comments that it is not clear what it means to divide by an interval that spans
zero. Modify Alyssa's code to check for this condition and to signal an error if
it occurs. |#


(define (div-interval-2.10 x y)
  (if (= 0 (width y))
      (error "cannot divide by interval with width of 0")
      (div-interval x y)))


;; Exercise 2.11.
#| In passing, Ben also cryptically comments: ``By testing the signs of the
endpoints of the intervals, it is possible to break mul-interval into nine cases,
only one of which requires more than two multiplications.'' Rewrite this procedure
using Ben's suggestion. |#

(define (pos? x) (> x 0))
(define (neg? x) (< x 0))

(define (mul-interval-2.11 x y)
  (let ((lb-x (lower-bound x))
        (lb-y (lower-bound y))
        (ub-x (upper-bound x))
        (ub-y (upper-bound y)))
    (if (pos? lb-x)
        (if (pos? ub-x)
            (if (pos? lb-y)
                (make-interval (* lb-x lb-y) (* ub-x ub-y))
                (if (pos? ub-y)
                    (make-interval (* ub-x lb-y) (* ub-x ub-y))
                    (make-interval (* ub-x lb-y) (* lb-x ub-y))
                )
            )
            (error "ub of x < lb of x")
        )
        (if (pos? ub-x)
            (if (neg? lb-y)
                (if (pos? ub-y)
                    (make-interval (min (* lb-x ub-y) (* ub-x lb-y))
                                    (max (* lb-x lb-y) (* ub-x ub-y)))
                    (make-interval (* ub-x lb-y) (* lb-x ub-y))
                )
                (mul-interval y x)
            )
            (if (and (neg? lb-y) (neg? ub-y))
                (make-interval (* ub-x ub-y) (* lb-x lb-y))
                (mul-interval y x)
            )
        )
    )
  )
)

(define mult-test (mul-interval interval-a interval-b))
(define mult-test-2.11 (mul-interval-2.11 interval-a interval-b))
(and (= (lower-bound mult-test) (lower-bound mult-test-2.11))
      (= (upper-bound mult-test) (upper-bound mult-test-2.11))) ; #t


;; Exercise 2.12.
#| After debugging her program, Alyssa shows it to a potential user, who complains
that her program solves the wrong problem. He wants a program that can deal with
numbers represented as a center value and an additive tolerance; for example, he
wants to work with intervals such as 3.5± 0.15 rather than [3.35, 3.65]. Alyssa
returns to her desk and fixes this problem by supplying an alternate constructor
and alternate selectors: |#

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
; (define (width i)
;   (/ (- (upper-bound i) (lower-bound i)) 2))

#| Unfortunately, most of Alyssa's users are engineers. Real engineering
situations usually involve measurements with only a small uncertainty, measured
as the ratio of the width of the interval to the midpoint of the interval.
Engineers usually specify percentage tolerances on the parameters of devices,
as in the resistor specifications given earlier.

Define a constructor make-center-percent that takes a center and a percentage
tolerance and produces the desired interval. You must also define a selector
percent that produces the percentage tolerance for a given interval. The center
selector is the same as the one shown above.|#

(define (make-center-percent c tol)
  (let ((width (* tol c)))
  (make-interval (- c width) (+ c width))))

(define (tolerance z)
  (let ((ub (upper-bound z))
        (lb (lower-bound z)))
  (/ (- ub lb) (+ ub lb))))

(define (approx? x y)
  (< (abs (- x y)) 0.0001)
)

(approx? (tolerance (make-center-percent 28.3 3.45)) 3.45) ; #t

;; Exercise 2.13.
#| Show that under the assumption of small percentage tolerances there is a simple
formula for the approximate percentage tolerance of the product of two intervals
in terms of the tolerances of the factors. You may simplify the problem by assuming
that all numbers are positive. |#

(define interval-c (make-center-percent 12.44 0.01))
(define interval-d (make-center-percent 93.44 0.07))
(approx? (tolerance (mul-interval interval-c interval-d))
         (+ (tolerance interval-c) (tolerance interval-d))) ; #t

;; Exercise 2.14.
#| After considerable work, Alyssa P. Hacker delivers her finished system. Several
years later, after she has forgotten all about it, she gets a frenzied call from
an irate user, Lem E. Tweakit. It seems that Lem has noticed that the formula for
parallel resistors can be written in two algebraically equivalent ways:

(R_1 R_2) / (R_1 + R_2)
and
1 / ((1/R_1) + (1/R_2))

He has written the following two programs, each of which computes the
parallel-resistors formula differently: |#

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

#| Lem complains that Alyssa's program gives different answers for the two ways of
computing. This is a serious complaint. Demonstrate that Lem is right. Investigate
the behavior of the system on a variety of arithmetic expressions. Make some
intervals A and B, and use them in computing the expressions A/A and A/B. You
will get the most insight by using intervals whose width is a small percentage
of the center value. Examine the results of the computation in center-percent
form (see exercise 2.12). |#

(define (c-par1 r1 r2)
  (let ((cr1 (center r1))
        (cr2 (center r2)))
          (/ (*  cr1 cr2)
                (+ cr1 cr2))))

(define (c-par2 r1 r2)
  (let ((cr1 (center r1))
        (cr2 (center r2)))
        (/ 1 (+ (/ 1 cr1) (/ 1 cr2)))
      ))

(c-par1 interval-c interval-d)
(c-par2 interval-c interval-d)

(tolerance (par1 interval-c interval-d))
(tolerance (par2 interval-c interval-d))

#| From the above results we can see that Lem is right. The problem is that the
centers of the intervals are the same but the tolerance of the result interval
(and hence its boundaries) are different. Exploring we see that when intervals
are added, multiplied, or divided, their tolerances add.

Hence, if t1 and t2 are the tolernce in percent of the two resistors respectively,
we multiply once (res tol: t1+t2), add once (res tol: t1+t2), and divide once (res
tol: 2*t1 + 2*t2).

Note that any operation with the interval (make-interval 1 1) doesn't change the
tolerance of the result interval as the tolerance of that interval is 0.
In the second formula we only add two intervals once (res tol: t1+t2).

Thus, we get the result that the centers of the resulting intervals from the two
forumals are the same, but the tolerances are |#


; (tolerance (add-interval interval-c interval-d))
; (+ (tolerance interval-c) (tolerance interval-d))
; (tolerance interval-c)
; (tolerance interval-d)
; (approx? (tolerance (add-interval interval-c interval-d))
;          (+ (tolerance interval-c) (tolerance interval-d)))

; (define (t-par1 r1 r2)
;   (let ((cr1 (center r1))
;         (cr2 (center r2)))
;           (/ (*  cr1 cr2)
;                 (+ cr1 cr2))))

; (define (t-par2 r1 r2)
;   (let ((cr1 (center r1))
;         (cr2 (center r2)))
;         (/ 1 (+ (/ 1 cr1) (/ 1 cr2)))
;       ))

(define α 7)
α
\alpha

;; Exercise 2.15.
#| Eva Lu Ator, another user, has also noticed the different intervals computed
by different but algebraically equivalent expressions. She says that a formula
to compute with intervals using Alyssa's system will produce tighter error bounds
if it can be written in such a form that no variable that represents an uncertain
number is repeated. Thus, she says, par2 is a ``better'' program for parallel
resistances than par1. Is she right? Why? |#

#| Yes, Eva is right. As stated in the previous exercise, when intervals with uncertainty
(or tolerance) are multiplied, divided, or added, their tolerances add. Hence,
using a formula with no variables that represent an uncertain number, the error
bounds will be tighter |#

;; Exercise 2.16.
#| Explain, in general, why equivalent algebraic expressions may lead to different
answers. Can you devise an interval-arithmetic package that does not have this
shortcoming, or is this task impossible? (Warning: This problem is very difficult.) |#

#| This is the dependency problem in interval arithmetic which states "If an interval
occurs several times in a calculation using parameters, and each occurrence is taken
independently then this can lead to an unwanted expansion of the resulting intervals."

It may be possible to solve this, but as the question states, it is very hard and
there is no viable solution yet. |#




