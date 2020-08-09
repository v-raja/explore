#lang sicp
(#%require (lib "27.ss" "srfi"))

(define (square x) (* x x))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (divides? a b)
  (= (remainder b a) 0))

; Exercise 1.21.  Use the smallest-divisor procedure to find the smallest divisor
; of each of the following numbers: 199, 1999, 19999.

; (smallest-divisor 19999) ; 7
; (smallest-divisor 1999)  ; 1999
; (smallest-divisor 199)   ; 199

(define (timed-prime-test n)
    (define (prime? n)
      (= n (smallest-divisor n)))
    (define (start-prime-test n start-time)
      (if (prime? n)
          (report-prime (- (runtime) start-time))
          #f))
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time)
      #t)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)


#|
Using this procedure, write a procedure search-for-primes that checks the primality
of consecutive odd integers in a specified range. Use your procedure to find the
three smallest primes larger than 1000, larger than 10,000, larger than 100,000,
larger than 1,000,000. Note the time needed to test each prime. Since the testing
algorithm has order of growth of (sqrt(n)), you should expect that testing for primes
around 10,000 should take about sqrt(10) times as long as testing for primes around 1000.
Do your timing data bear this out? How well do the data for 100,000 and 1,000,000
support the sqrt(n) prediction? Is your result compatible with the notion that programs
on your machine run in time proportional to the number of steps required for the
computation? |#

(define (find-next-primes primes x)
    (define (recur num primes-found)
        (if (not (= primes-found primes))
          (if (timed-prime-test num)
            (recur (+ 2 num) (+ 1 primes-found))
            (recur (+ 2 num) primes-found)
          )
        )
    )
  (if (even? x)
    (recur (+ 1 x) 0)
    (recur x 0)
  )
)

; (find-next-primes 3 1000)
; (find-next-primes 3 10000)
; (find-next-primes 3 100000)
; (find-next-primes 3 1000000)

#|
Results (time in microseconds):
n       | t(p1) | t(p2) | t(p3)
1000    | 1     | 1     | 1
10000   | 3     | 4     | 5
100000  | 14    | 10    | 13
1000000 | 36    | 36    | 48

sqrt(10) = 3.16... so yes, these results support the O(sqrt(n)) prediction
as when n grows by 10x, the time grows by approx. sqrt(10)x. This result is
compatibale with the notion that programs run in the time proportional to the
number of steps required for the computation, and the variation in times are
exceptions that prove the rule. |#

; Exercise 1.23.
; Define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2.
; Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1).


(define (smallest-divisor-1.23 n)
  (define (next x)
    (if (= x 2) 3 (+ x 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (timed-prime-test-1.23 n)
    (define (prime? n)
      (= n (smallest-divisor-1.23 n)))
    (define (start-prime-test n start-time)
      (if (prime? n)
          (report-prime (- (runtime) start-time))
          #f))
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time)
      #t)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

; With timed-prime-test incorporating this modified version of smallest-divisor,
; run the test for each of the 12 primes found in exercise 1.22.
; (map timed-prime-test-1.23 '(1009 1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037))

#|
Results (time in microseconds):
n       | t(p1) | t(p2) | t(p3)
1000    | 1     | 1     | 0
10000   | 1     | 2     | 1
100000  | 4     | 4     | 5
1000000 | 12    | 13    | 16
|#

; Since this modification halves the number of test steps, you should expect it to run
; about twice as fast. Is this expectation confirmed? If not, what is the observed ratio
; of the speeds of the two algorithms, and how do you explain the fact that it is
; different from 2?

; The ratio of the speeds is between 1.5 and 2. So yes, the expectation is confirmed.
; The ratio is not strictly 2 because the increment function is likely optimized by the
; compiler compared to our userdefined function next. Further, in our function there is
; an extra step to evaluate the if statement.


; Exercise 1.24.  Modify the timed-prime-test procedure of exercise 1.22 to use
; fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise.
; Since the Fermat test has (log n) growth, how would you expect the time to test primes near
; 1,000,000 to compare with the time needed to test primes near 1000?

; So recall that Fermat test is log n growth because the expmod function uses successive squaring
; which divides the power n of a^n by 2 on subsequent calls. Hence, the growth is log base 2 of n.
; Thus, since log base 2 of 1000n = log_2{1000} + log_2{n} = c + log_2{n}, I expect the the time
; to test primes near 1,000,000 to be the time needed to test primes near 1,000 plus some constant
; (we can't calcuate this constant as it depends on factors like the compiler, etc.).
; Further, this constant c will be exactly 3d where d is the constant amount of time more required
; to test primes near 10,000 compared to 1,000.

(define (fast-prime? n times)
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random-integer (- n 1)))))
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (timed-prime-test-1.24 n)
    (define (prime? n)
      (= n (smallest-divisor n)))
    (define (start-prime-test n start-time)
      (if (fast-prime? n 1000)
          (report-prime (- (runtime) start-time))
          #f))
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time)
      #t)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

; (map timed-prime-test-1.24 '(1009 1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037))

#|
Results (time in microseconds):
n       | t(p1) | t(p2) | t(p3)
1000    | 53    | 54   | 55
10000   | 67    | 62   | 65
100000  | 74    | 75   | 81
1000000 | 88    | 87    | 88
|#

; Thus, the constant c here is about 30 us, and results are as expected. A more
; influential factor on the running time of this algorithm is the number of tries
; of fast-prime. If t is the number of tries, the number of steps, and hence time,
; grows as O(t).


; Exercise 1.25.  Alyssa P. Hacker complains that we went to a lot of extra work in
; writing expmod. After all, she says, since we already know how to compute exponentials,
; we could have simply written
;                   (define (expmod-1 base exp m)
;                     (remainder (fast-expt base exp) m))
; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

#|
This is the original expmod.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
            (square (expmod base (/ exp 2) m))
            m))
        (else
         (remainder
            (* base (expmod base (- exp 1) m))
            m))))

No, I don't think Alyssa's procedure would serve well for our fast prime tester.
Her function will compute the entire value of the exponenet before caluclating
the remainder. This will be a bignum operation and depending on whether bignum
operations are carried out using hardware registers or software, this will be
an expensive operation. Furthermore, we would have to store the result of a very
large integer. For example, using Alyssa's algo, fast-prime? would have to compute
(expmod b 1009 1009) which would inturn have to compute (fast-expt b 1009) for the
smallest prime number above 1000, where 0 < b < 1009. The expected value
of b is 504, so using that the result of (fast-expt 504 1009) is
`565472219179729582345737259674459783937727036404966492305690219354495975127770713763197180940070106276105675081388178986723435637398195034054450057304258863947408482035046301017498892291518461957104760780673263642559967752382639022586965268801960792427641464138703940798429683095983757498738633647668289100674662854863685291899950195863594300611220897655459707252510234987121884648007411371925108258872688109916687201839218462256744244037425283856429977073770987885354845644209580865414346899007364713672105147123935591924242241456693358668522113023321151564182283964563984459268531053648032249557622925100576681275447247058799491597362071907526828347834896010850754359481869840613093797492786615467210677442037803264807013258585593547938931306087462241487756439472370194795900882513763352844212011537903065386200000963896349517706549554864887957075779921231575544214714276571991160589581160993217010054816953915163246307298227494911340510598180482250597411478503529557719798883674065195240116679857665224739899855259519544545613163341493378105175935174893590793888164957259529025165193958311486014687832692963645006580246718751700955079969514433114026439270743802836768510810270233290655787886094352779200718650999076017994084343164625556988524261116709082766314801605366701458981217123527497097139682819071964356445060182553822425678324562059239817565762838503042519621699055988966725528130265446512591354701696380216015203031725582574813395948252126053885122986728620407584549951217931999083527637166498737530935998489753301239881789962243661271100487035691089766652613352239443594178061306714458488675806569759770200146131532682453012681936097109260577889266793760486883357561216758218447284687472758905022065681084101349613068494715666122287576599693794704453191762836440956837882490941120684629315195603359029475805303565840699216354296014378137397575108659510003878806958279398175355527632646046379616252011253168641430303064298324018996828880028848892315383890447198995424977722414466651813483845569386915682841683278743827836186806278639944532484291790433901628468121123754920160977889572487621498300997149870789343236421033145807656158288810479237981084283918952231995873055348927559740440205383866452042003340115642654778342816782002035599468703823866714498923550684836641498005828488636526436547410063205531282440611126466859883967338650358141470127062041989174952945222221012749034786630698587344606023673203947464210355380872327701312445642789806856347948281367960492631527134820588623063864988829214180384703749216212293273738378227604539426590110400666917757452518656223470538303410605389603049512227961219658562401192203274346366284933753431293876924734204133601703577327251052195910303653285448254246715654144`
which would take (quotient (log (fast-expt 504 1009) 2) 8) = 1,132 bytes to store
and this is on the lower end.
(define (num-bytes n) (quotient (floor (log n 2)) 8))
Doing the same exercise for the first prime above 1,000,000:
(num-bytes (fast-expt (quotient 1000003 2) 1000003)) = 2,366,453 bytes on average
For the first prime above 1,000,000,000:
(num-bytes (fast-expt (quotient 1000000007 2) 1000000007)) =

On the other hand, the original expmod recursive calls itself breaking down the
orignial problem into smaller ones until it's small a very small exponent and
immediately getting the remainder keeps the number smaller than n upon when
walking up the tree to the root from the node. |#


;; Exercise 1.26.
#| Louis Reasoner is having great difficulty doing exercise 1.24.
His fast-prime? test seems to run more slowly than his prime? test. Louis calls
his friend Eva Lu Ator over to help. When they examine Louis's code, they find
that he has rewritten the expmod procedure to use an explicit multiplication,
rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

``I don't see what difference that could make,'' says Louis. ``I do.'' says Eva.
 ``By writing the procedure like that, you have transformed the (log n) process
 into a (n) process.'' Explain.

The function (square (expmod base (/ exp 2) m)) evaluates the expression
(expmod base (/ exp 2) m) once and then multiplies the evaluated result by itself.
The expression (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) evaluates
(expmod base (/ exp 2) m) two times separately and then multiples their results.
With square, the recurrence relation describing the amount of work done, T(n), is
T(n) <= T(n/2) + ___ where ___ is the work done in squaring the result of (expmod base (/ exp 2) m).
Now, with Louis' code, the recurrence relation of the work done is
T(n) <= 2T(n/2) + ___ where ___ is the same amount of work done as above for squaring the result.

Thus, the first relation is clearly O(log n) as we're halving the problem size on
each recursive call. But, the second relation is just O(n) as we're halving the problem
size but calling (expmod) twice negates any benefit from halving the problem size.
 |#

;; Exercise 1.27.
#| Demonstrate that the Carmichael numbers listed in footnote 47 really do fool
the Fermat test. That is, write a procedure that takes an integer n and tests whether
an is congruent to a modulo n for every a<n, and try your procedure on the given
Carmichael numbers. |#

(define (try-all-fermat n)
  (define (recur count)
    (cond ((= count n) #t)
          ((= (expmod count n n) count) (recur (+ 1 count)))
          (else #f)
    )
  )
  (recur 1)
)

; (map try-all-fermat '(561 1105 1729 2465 2821 6601))
; (map try-all-fermat '(1009 1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037))

;; Exercise 1.28.
#| One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test
(Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's Little
 Theorem, which states that if n is a prime number and a is any positive integer less
than n, then a raised to the (n - 1)st power is congruent to 1 modulo n. To test
the primality of a number n by the Miller-Rabin test, we pick a random number a<n
and raise a to the (n - 1)st power modulo n using the expmod procedure. However,
whenever we perform the squaring step in expmod, we check to see if we have
discovered a ``nontrivial square root of 1 modulo n,'' that is, a number not equal
to 1 or n - 1 whose square is equal to 1 modulo n. It is possible to prove that if
such a nontrivial square root of 1 exists, then n is not prime. It is also possible
to prove that if n is an odd number that is not prime, then, for at least half the
numbers a<n, computing a^(n-1) in this way will reveal a nontrivial square root of 1
modulo n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod
procedure to signal if it discovers a nontrivial square root of 1, and use this to
implement the Miller-Rabin test with a procedure analogous to fermat-test. Check
your procedure by testing various known primes and non-primes. Hint: One convenient
way to make expmod signal is to have it return 0. |#

; Only square trivial roots (1 or m-1), else ret 0
(define (square-trivial x m)
    (if (or (= x 1) (= x (- m 1)))
        1
        0
    )
)

(define (expmod-1.28 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-trivial (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (try-all-fermat-1.28 n)
  (define (recur a)
    (cond ((= a n) #t)
          ((= (expmod-1.28 a (- n 1) n) 1) (recur (+ 1 a)))
          (else #f)
    )
  )
  (recur 1)
)

(map try-all-fermat-1.28 '(561 1105 1729 2465 2821 6601))
(map try-all-fermat-1.28 '(1009 1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037))
