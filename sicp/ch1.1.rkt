#lang sicp

;; Exercise 1.2
#|
(/ ((+ 5
      4
       (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3
      (- 6 2)
      (- 2 7)))
|#

;; Exercise 1.3
(define square (lambda (x) (* x x)))
(define sos (lambda (x y)
    (+ (square x) (square y))))
(define sos_largest_two (lambda (x y z)
  (cond ((and (< z y) (< z x)) (sos x y))
        ((and (< x z) (< x y)) (sos z y))
        (else (sos x z)))))
(define a 4)
(define b (- 6))
(define c 8)
(= (sos_largest_two a b c)
   (sos_largest_two a c b)
   (sos_largest_two c b a)
   (sos_largest_two c a b)
   (sos_largest_two b c a)
   (sos_largest_two b a c)
   (sos c a))

;; Exercise 1.4
; The if-expression evaluates to '+' or '-' which are symbols
; for a lambda functions that take two arguments and adds/subtracts them

;; Exercise 1.5
; For a system that uses applicative order evaluation, nothing will run
; because (test 0 (p)) will evaluate (p) which will lead to an infinite
; loop. For normative-order, we will see 0 because (p) isn't evaluated
; and isn't the result of the conditional expression so will never be
; evaluated

;; Exercise 1.6
; Since interpreters use aplicative-order evaluation,
; it'll try to evluate the else-clause which recursively
; calls sqrt-iter which will then try to evaluate the else-clause
; which will recursively call itself. Thus, no value is resolved
; and this leads to an infinite loop

;; Exercise 1.7
(define (average x y)
    (/ (+ x y) 2))

(define (sqrt x)
    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.0000001))
    (sqrt-iter 1.0))

(define (new-sqrt x)
    (define (sqrt-iter guess prev-guess)
        (if (good-enough? guess prev-guess)
            guess
            (sqrt-iter (improve guess) guess)))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (good-enough? guess prev-guess)
        (< (abs (- guess prev-guess)) 0.0000001))
    (sqrt-iter 1.0 0))
; Defining good-enough? to be the difference between guesses is a lot more
; accurate. The limited precision of computers means the decimals are only
; accurate up to a certain decimal place. Thus, with the previous good-enough?
; square and comparing the difference is only accurate to a few decimal places
; and we can't get a sqrt method accurate to more than a few decimal places.
; Further, the original sqrt function is very inaccurate for very small numbers
; because the value in good-enough? can be a lot large enough than the actual
; square root of small numbers that cause the process to terminate early with
; an inaccurate value

(define (cmp-sqrt x) (- (sqrt x) (new-sqrt x)))
(cmp-sqrt 0)
(cmp-sqrt 2)
(cmp-sqrt 10)
(cmp-sqrt 999)
(cmp-sqrt 0.2)
(cmp-sqrt 0.0001)
(cmp-sqrt 0.00000000000000001)
(cmp-sqrt 94922)
(cmp-sqrt 10232222)

;; Exercise 1.8
(define (cubert x)
    (define (cubert-iter guess prev-guess)
        (if (good-enough? guess prev-guess)
            guess
            (cubert-iter (improve guess) guess)))
    (define (improve guess)
        (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (define (good-enough? guess prev-guess)
        (< (abs (- guess prev-guess)) 0.0000001))
    (cubert-iter 1.0 0))

(cubert 8)
(cubert 125)



