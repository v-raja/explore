#lang sicp

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Exercise 1.40.
#| Define a procedure cubic that can be used together with the newtons-method
procedure in expressions of the form
          (newtons-method (cubic a b c) 1)
to approximate zeros of the cubic x^3 + ax^2 + bx + c. |#

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c))
)
(define (solve-cubic a b c)
  (newtons-method (cubic a b c) 1.0)
)

; (= ((cubic 3 3 1) (solve-cubic 3 3 1)) 0)                 ; #t
; (= ((cubic (- 2) (- 5) 6) (solve-cubic (- 2) (- 5) 6)) 0) ; #t

;; Exercise 1.41.
#| Define a procedure double that takes a procedure of one argument as argument
and returns a procedure that applies the original procedure twice. For example,
if inc is a procedure that adds 1 to its argument, then (double inc) should be a
procedure that adds 2. |#

(define (double y)
  (lambda (x) (y (y x)))
)

(define (inc x) (+ 1 x))

; What value is returned by (((double (double double)) inc) 5)?
#|
;; (double double)
(lambda (x1) (double (double x1)))
;; (double (double double))
(double (lambda (x1) (double (double x1))))
(lambda (x3) ((lambda (x2) (double (double x2))) ((lambda (x1) (double (double x1))) x3)))
;; (double (double double)) inc
(lambda (x3) ((lambda (x2) (double (double x2))) ((lambda (x1) (double (double x1))) x3))) inc
(lambda (x2) (double (double x2))) ((lambda (x1) (double (double x1))) inc)
(lambda (x2) (double (double x2))) (double (double inc))
(double (double (double (double inc))))
(double (double (double (+ 2 x))))
(double (double (+ 4 x)))
(double (+ 8 x))
(+ 16 x)
Thus, (+ 16 5) = 21 is returned |#

;; Exercise 1.42.
#| Let f and g be two one-argument functions. The composition f after g is defined
to be the function x  f(g(x)). Define a procedure compose that implements composition.
For example, if inc is a procedure that adds 1 to its argument,
        ((compose square inc) 6)
        49 |#

(define (compose f g)
  (lambda (x) (f (g x)))
)
; ((compose square inc) 6) ; 49



;; Exercise 1.43.
#| If f is a numerical function and n is a positive integer, then we can form the
nth repeated application of f, which is defined to be the function whose value at
x is f(f(...(f(x))...)). For example, if f is the function x   x + 1, then the nth
repeated application of f is the function x   x + n. If f is the operation of
squaring a number, then the nth repeated application of f is the function that
raises its argument to the 2nth power. Write a procedure that takes as inputs a
procedure that computes f and a positive integer n and returns the procedure that
computes the nth repeated application of f. Your procedure should be able to be
used as follows:
    ((repeated square 2) 5)
    625 |#

(define (repeated f n)
    (if (= n 1)
        f
        (repeated (compose f f) (- n 1))
    ))

; ((repeated square 2) 5) ; 625

(define (repeated-iter f n)
    (define (iter g i)
      (if (= i 1)
          g
          (iter (compose f g) (- i 1))
    ))
  (iter f n)
)
; ((repeated-iter square 2) 5) ;625


;; Exercise 1.44.
#| The idea of smoothing a function is an important concept in signal processing.
If f is a function and dx is some small number, then the smoothed version of f is
the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx).
Write a procedure smooth that takes as input a procedure that computes f and returns a
procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth
a function (that is, smooth the smoothed function, and so on) to obtained the n-fold
smoothed function. Show how to generate the n-fold smoothed function of any given
function using smooth and repeated from exercise 1.43. |#

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0))
)

(define (smooth-n f n)
  ((repeated smooth n) f)
)


;; Exercise 1.45.
#| We saw in section 1.3.3 that attempting to compute square roots by naively finding
a fixed point of y  x/y does not converge, and that this can be fixed by average
damping. The same method works for finding cube roots as fixed points of the
average-damped y  x/y^2. Unfortunately, the process does not work for fourth roots --
a single average damp is not enough to make a fixed-point search for y  x/y^3 converge.
On the other hand, if we average damp twice (i.e., use the average damp of the average
damp of y  x/y^3) the fixed-point search does converge. Do some experiments to
determine how many average damps are required to compute nth roots as a fixed-point
search based upon repeated average damping of y  x/y^(n-1). Use this to implement a
simple procedure for computing nth roots using fixed-point, average-damp, and the
repeated procedure of exercise 1.43. Assume that any arithmetic operations you need
are available as primitives. |#

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (expt base n)
    (define (iter base n res)
        (cond ((= n 0) res)
              ((even? n) (iter (square base) (/ n 2) res))
              (else (* base (iter base (- n 1) res)))))
  (iter base n 1))

(define (nth-root n x)
  (let ((a (if (< (- n 2) 1) 1 (- n 2))))
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp a)
                            1.0)))
; (nth-root 2 25)
; (nth-root 3 (expt 27 3))
; (nth-root 3 (expt 44 3))
; (nth-root 4 (expt 27 4))
; (nth-root 5 (expt 233 5))
; (nth-root 6 (expt 839 6))


;; Exercise 1.46.
#| Several of the numerical methods described in this chapter are instances of an
extremely general computational strategy known as iterative improvement.
Iterative improvement says that, to compute something, we start with an initial
guess for the answer, test if the guess is good enough, and otherwise improve the
guess and continue the process using the improved guess as the new guess. Write a
procedure iterative-improve that takes two procedures as arguments: a method for
telling whether a guess is good enough and a method for improving a guess.
Iterative-improve should return as its value a procedure that takes a guess as
argument and keeps improving the guess until it is good enough. Rewrite the sqrt
procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms
of iterative-improve. |#

(define (iterative-improve good-enuf? improve)
  (define (iter guess)
        (if (good-enuf? guess)
            guess
            (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt x)
    (define (improve guess)
      (average guess (/ x guess)))
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

; (sqrt 64)
; (sqrt 49)

(define (fixed-point-1.46 f first-guess)
  (define (close-enough? g1)
    (< (abs (- g1 (f g1))) tolerance))
  ((iterative-improve close-enough? f) first-guess)
)

;; To test fixed-point-1.46
; (define (newtons-method-1.46 g guess)
;   (fixed-point-1.46 (newton-transform g) guess))
; (define (solve-cubic-1.46 a b c)
;   (newtons-method-1.46 (cubic a b c) 1.0))
; (= ((cubic 3 3 1) (solve-cubic-1.46 3 3 1)) 0)                 ; #t
; (= ((cubic (- 2) (- 5) 6) (solve-cubic-1.46 (- 2) (- 5) 6)) 0) ; #t
