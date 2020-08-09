#lang sicp
; (provide (all-defined-out))

;; Exercise 1.9

; The first process is recursive, second iterative.

;; Exercise 1.10

#|
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) is
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024 (This is 2^10)


(A 2 4) is
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
By interpolation from above, this should be 2^16 = 65536

((2^x)^y) maybe
(A 2 3) became 16
(A 2 2) became 4
(A 2 1) became 2

(A 3 3) is
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
By interpolation, (A 2 2) is 4
(A 2 4)
From above, 2^16 = 65536

(A 1 65536) = (A 2 5)
(A 1 16)    = (A 2 4) = (A 3 3) = 2^16 = 4^8 = 16^4   = 65536
(A 1 4)     = (A 2 3)           = 2^4  = 4^2 = 16^1   = 16
(A 1 2)     = (A 2 2) = (A 3 2) = 2^2  = 4^1 = 16^0.5 = 4    ; because of base case of (= 1 y)

(A 2 n) = 2^(A 2 (n-1))
        = 2^2^(A 2 (n-2))
        = 2^2^2^......2^(A 2 2)
        = 2^2^(n-2 times)4
 2 4) = 2^(A 2 3)
        = 2^2^(A 2 2)
(2^(n-1))^2

(define (f n) (A 0 n)) computes 2*n
(define (g n) (A 1 n)) computes 2^n
(define (h n) (A 2 n)) computes 16^(n-2)

(A 2 n) = (A 2 n-1)^n
(A 2 4) = (A 2 3)^4 = ((A 2 2)^3)^4


(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1) ; makes sense; we reach here with we use the largest denomination we have to make this
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(cc (- amount (first-denomination kinds-of-coins))
    kinds-of-coins)
; 1 if you can make total amount with largest denomination only
(cc amount (- kinds-of-coins 1))
; number of ways you can make denomination with all other coins

for 1
                                                                                (cc 100 5)
(+                                         (cc 100 4)                                                                     (cc 50 5))
(+                   (cc 100 3)                                 (cc 75 4)                                  (cc 50 4)                  (cc 0 5))
(+        (cc 100 2)          (cc 90 3)            (cc 75 3)              (cc 50 4)            (cc 50 3)              (cc 25 4)       (cc 0 5))
(+ (cc 100 1) (cc 95 2)  (cc 90 2) (cc 80 3)  (cc 75 2) (cc 65 3)   (cc 50 3) (cc 25 4)   (cc 50 2) (cc 40 3)    (cc 25 3) (cc 0 4)   (cc 0 5))

(cc 10 2)
(+ (cc 10 1) (cc 5 2))
(+ (cc 10 1) (cc 5 2))


(cc 10 1)
(+ (cc 10 0) (cc 9 1))
(+ (cc 9 1))
(+ (cc 9 0) (cc 8 1))

(cc 5 2)
(+ (cc 5 1) 1)
(+ (+ (cc 4 1)) 1)


|#

;; Exercise 1.11.
; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.
; Write a procedure that computes f by means of a recursive process.
(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1)) (* (f-rec (- n 2)) 2) (* (f-rec (- n 3)) 3))
  )
)
; Write a procedure that computes f by means of an iterative process.
(define (f n)
  (define (f-recur curr prev prevprev count)
    (if (< n count)
      curr
      (f-recur (+ curr (* prev 2) (* 3 prevprev)) curr prev (+ count 1))
    )
  )
  (if (< n 3)
      n
      (f-recur 2 1 0 3)
  )
)

;; Exercise 1.12.
; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

(define (second xs)
  (car (cdr xs))
)

(define (pascal lines)
  (define (calc-pascal xs)
    (if (< (length xs) 2)
      '()
      (cons (+ (car xs) (second xs)) (calc-pascal (cdr xs)))))
  (define (pascal-recur xs count)
    (if (< count lines)
      (begin
        (display xs)(display "\n")
        (pascal-recur  (append '(1) (calc-pascal xs) '(1)) (+ count 1)))))
  (pascal-recur '(1) 0))


;; Exercise 1.13

(define (fib n)
  (define (fib-recur curr next count)
    (if (> count n)
        curr
        (fib-recur next (+ curr next) (+ count 1))
    )
  )
  (fib-recur 0 1 1)
)

; Exercise 1.13.  Prove that Fib(n) is the closest integer to n/5, where  = (1 + 5)/2.
; Hint: Let  = (1 - 5)/2. Use induction and the definition of the Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = (n - n)/5.
; First prove the second statement through induction (key: \phi = 1 + 1/\phi and \psi = 1 + 1/\psi)
; Given first statement, we have to show abs(\psi^k/sqrt(5)) < 0.5. for k = 1, it's 1/sqrt(5),
; and it's a geometric series with abs(r) < 1 so converges to 0 for large k by alternating series test (or common sense, you choose)

; Exercise 1.14.  Draw the tree illustrating the process generated by the count-change procedure of section 1.2.2 in making change for 11 cents.
; What are the orders of growth of the space and number of steps used by this process as the amount to be changed increases?

; Done in ex1.14.md

; Exercise 1.15.  The sine of an angle (specified in radians) can be computed by making use of the approximation sin x  x if x is sufficiently small, and the trigonometric identity

; a.  How many times is the procedure p applied when (sine 12.15) is evaluated?
#|
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
(p (p (p (p 0.1495))))
...
p is applied 5 times. In general, p is applied the number of time we have to
divide a by 3 for the result of the division to be smaller than 0.1.
|#

; b.  What is the order of growth in space and number of steps (as a function of a)
; used by the process generated by the sine procedure when (sine a) is evaluated?
#|
The number of times we have to divide a by 3 for the result of the division to be
less than 0.1 is log_3{10a}. (log_3{a} will give us the number of time we have divide
a by 3 for it to be less than 1, and thus log_3{10a} is the number of times we have to
divide a by 3 for it to be less than 0.1).

The maximum space is use for remembering the number of time we have to apply p after
`sine` evaluated to a number. Thus, we have to remember to apply p log_3{10a} times.
Supressing constant factors, we see that space grows as O(log{a}).

Number of steps:
Everytime sine doesn't evalaute to a number, we have to do a division (thus, log_3{10a} divisions in total).
Everytime p is called, we do 4 divisions, 1 subtraction, and 5 multpilcations. A total of 10 ops.
Thus, we do a total of log_3{10a} + log_3{10a}(10) = 11 (log_3{10a}). Thus, O(log{a}).
|#

; Exercise 1.16.  Design a procedure that evolves an iterative exponentiation process
; that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.

(define (square x)
  (* x x)
)

(define (expt x b)
  (define (expt-iter num pow prod)
      (cond ((= 1 pow) (* prod num))
        ((even? pow) (expt-iter (square num) (/ pow 2) prod))
        (else (expt-iter num (- pow 1) (* prod num)))
      )
  )
  (if (= b 0)
    1
    (expt-iter x b 1)
  )
)

#|
Exercise 1.17.  The exponentiation algorithms in this section are based on performing
exponentiation by means of repeated multiplication. In a similar way, one can perform
integer multiplication by means of repeated addition. The following multiplication
procedure (in which it is assumed that our language can only add, not multiply) is
analogous to the expt procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

This algorithm takes a number of steps that is linear in b. Now suppose we include,
together with addition, operations double, which doubles an integer, and halve,
which divides an (even) integer by 2. Using these, design a multiplication procedure
analogous to fast-expt that uses a logarithmic number of steps.
|#

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))
  )
)

; Exercise 1.18.  Using the results of exercises 1.16 and 1.17, devise a procedure
; that generates an iterative process for multiplying two integers in terms of adding,
; doubling, and halving and uses a logarithmic number of steps.

(define (mult-iter a b)
  (define (recur x y sum)
    (cond ((= y 0) sum)
        ((even? y) (recur (double x) (halve y) sum))
        (else (recur x (- y 1) (+ sum x)))
    )
  )
  (recur a b 0)
)

(if (= b 0)
    a
    (gcd b (remainder a b)))


(gcd 206 40)

(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
(if #f       206 (gcd 40 (remainder 206 40))
(gcd 40 (remainder 206 40)

(gcd 40 6)
(if (= 6 0) 40 (gcd 6 (remainder 40 6)))
(if #f      40 (gcd 6 (remainder 40 6)))
(gcd 6 (remainder 40 6))

(gcd 6 4)
(if (= 4 0) 6 (gcd 4 (remainder 6 4)))
(if #f      6 (gcd 4 (remainder 6 4)))
(gcd 4 (remainder 6 4))

(gcd 4 2)
(if (= 2 0) 4 (gcd 2 (remainder 4 2)))
(if #f      4 (gcd 2 (remainder 4 2)))
(gcd 2 (remainder 4 2))

(gcd 2 0)
(if (= 0 0) 2 (gcd 0 (remainder 2 0)))
(if #t      2 (gcd 0 (remainder 2 0)))
2


(define (find-next-3-primes n)
    (define (recur num primes-found)
        (cond ((= primes-found 3) "done")
              ((timed-prime-test num) (recur (+ 2 num) (+ 1 primes-found)))
              (else (recur (+ 2 num) primes-found))
        )
    )
  (if (even? n)
    (recur (+ 1 n) 0)
    (recur n 0)
  )
)
