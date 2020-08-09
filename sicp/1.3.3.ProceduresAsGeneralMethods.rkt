#lang sicp

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Exercise 1.35.
#|Show that the golden ratio  (section 1.2.2) is a fixed point of the transformation
x   1 + 1/x, and use this fact to compute  by means of the fixed-point procedure. |#

; (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0) ; 1.6180327868852458


;; Exercise 1.36.
#| Modify fixed-point so that it prints the sequence of approximations it generates,
using the newline and display primitives shown in exercise 1.22. Then
find a solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x).
Compare the number of steps this takes with and without average damping. |#

(define (average x y) (/ (+ x y) 2))

; (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.01)             ; 40 steps
; (newline)
; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.01) ; 16 steps

;; Exercise 1.37

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ res (d i))))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

; k has to be as large as 11 to be accurate to 4 decimal places

(define (cont-frac-recur n d k)
    (define (recur i)
      (if (> i k)
          0
          (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

;; Exercise 1.38.
#| In 1737, the Swiss mathematician Leonhard Euler published a memoir
De Fractionibus Continuis, which included a continued fraction expansion for e - 2,
where e is the base of the natural logarithms. In this fraction, the Ni are all 1,
and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program
that uses your cont-frac procedure from exercise 1.37 to approximate e, based on
Euler's expansion. |#


(define (d i)
    (if (= 0 (remainder (- i 2) 3))
        (* 2.0 (+ 1.0 (quotient (- i 2.0) 3.0)))
        1.0))
(define e
  (+ 2 (cont-frac (lambda (x) 1) d 100)))
e ; 2.7182818284590455

(define (square x) (* x x))
(define (tan-cf x k)
  (define (tan-d i)
    (+ 1.0 (* 2.0 (- i 1.0)))
  )
  (cont-frac
      (lambda (i) (if (= i 1) x (- (square x))))
      tan-d
      k))

(tan-cf 5 100) ; -3.3805150062465867


