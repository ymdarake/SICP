#lang racket

;;Procedures As Arguments

;ex. sigma notation
(define (sum f start next end)
  (if (> start end)
      0
      (+ (f start)
         (sum f (next start) next end))))

(define (inc n) (+ n 1))
(define (square n) (* n n))
(define (average x y) (/ (+ x y) 2))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (smallest-div n) 
  (define (divides? a b) 
    (= 0 (remainder b a))) 
  (define (find-div n test) 
    (cond ((> (square test) n) n) ((divides? test n) test) 
          (else (find-div n (+ test 1))))) 
  (find-div n 2)) 
(define (prime? n) 
  (if (= n 1) false (= n (smallest-div n))))
(define (gcd m n)
  (cond ((< m n) (gcd n m))
        ((= n 0) m)
        (else (gcd n (remainder m n)))))
(define (relative-prime? m n)
  (= (gcd m n) 1))


(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

;Exercise1.29
(define (integral f a b n)
  (define (next-2h x) (+ x (* 2 h)))
  (define h (/ (- b a) n))
  (* (/ h 3.0)
     (+ (f a)
        (f b)
        (* 4 (sum f (+ a h) next-2h b))
        (* 2 (sum f (next-2h a) next-2h (- b h))))))

;Exercise1.30
(define (sum-iter f start next end)
  (define (iter current result)
    (if (> current end)
        result
        (iter (next current) (+ result (f current)))))
  (iter start 0))

;Exercise1.31
(define (product-recur f current next end)
  (if (> current end)
      1
      (* (f current) (product-recur f (next current) next end))))

(define (product f current next end)
  (define (iter current acc)
    (if (> current end)
        acc
        (iter (next current) (* acc (f current)))))
  (iter current 1))

(define (pi-wallis-product accuracy)
  (define (pi-term n) 
    (if (even? n)
        (/ (+ n 2) (+ n 1)) 
        (/ (+ n 1) (+ n 2))))
  (* 4 (product pi-term 1 inc accuracy)))

;Exercise1.32
;left fold
(define (accumulate combiner null-value f start next end)
  (define (iter current acc)
    (if (> current end)
        acc
        (iter (next current) (combiner acc (f current)))))
  (iter start null-value))

;right fold
(define (accumulate-recur combiner null-value f start next end)
  (if (> start end)
      null-value
      (combiner (f start)
                (accumulate-recur combiner null-value f (next start) next end))))

(define (sum-accumulate f start next end)
  (accumulate + 0 f start next end))
(define (product-accumulate f start next end)
  (accumulate * 1 f start next end))

;Exercise1.33
(define (filtered-accumulate combiner null-value f start next end predicate)
  (define (iter current acc)
    (if (> current end)
        acc
        (iter (next current) (combiner acc
                                       (if (predicate current)
                                           (f current)
                                           null-value)))))
  (iter start null-value))
  
(define (sum-of-prime-squares start end)
  (filtered-accumulate + 0 square start inc end prime?))

(define (product-of-relative-primes n)
  (define (filter x) (relative-prime? x n))
  (filtered-accumulate * 1 identity 1 inc n filter))

;;; lambda, and let

;;lambda
(define (plus4 x) (+ x 4))
;is equivalent to
;(define plus4 (lambda (x) (+ x 4)))

;;A let expression is simply syntactic sugar for the underlying lambda expression
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
;is equivalent to
(define (f-let-sample x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;Exercise1.34
(define (f-1-34 g) (g 2))
;(f f)
;(f 2)
;(2 2)
;error! 2 is not a procedure.

;;; 1.3.3 Procedures as General Methods
; half-interval method: find the value of x where f(x) = 0. f(a) < 0 < f(b).
(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (define (close-enough? x y)
      (< (abs (- x y)) 0.001))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value) (search f neg-point mid-point))
                ((negative? test-value) (search f mid-point pos-point))
                (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; use 'average damping' to aid the convergence of fixed-point searches.
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;;Exercise 1.35
; x = 1 + 1/x  <=> x^2 - x - 1 = 0, x = (1 +- 5^1/2)
(define (calc-golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
; using 'average damping'
(define (calc-golden-ratio-average-damping)
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

;;Exercise 1.36
(define (log-with-average-damping); 9 steps to converge
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
(define (log-without-average-damping); 34 steps.
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

;;Exercise 1.37
(define (cont-frac n d k)
  (define (recur current)
    (if (= current k)
        (/ (n current) (d current))
        (/ (+ (d current) (recur (+ 1 current))))))
  (recur 1))
(define (cont-frac-iter n d k)
  (define (iter acc current)
    (if (= current 1)
        acc
        (iter (/ (n (- current 1)) (+ (d (- current 1)) acc)) (- current 1))))
  (iter (n k) k))
     
;;Exercise 1.38
(define (e-2 to-term-k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) (cond ((= 2 (remainder i 3)) (* 2 (/ (+ i 1) 3))); Di = 2N where N stands for 'Nth group' (divide three by three)
                               (else 1)))
             to-term-k))
;;Exercise 1.39
(define (tan-cf x k)
  (let ((minus-square-of-x (- (* x x))))
    (cont-frac (lambda (i) (if (= i 1) x minus-square-of-x))
               (lambda (i) (- (* 2 i) 1))
               k)))

;;; 1.3.4 Procedures as Returned Values
(define (average-damp f)
  (lambda (x) (average x (f x))))

; derivative
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;;;;; As programmers, we should be alert to opportunities
;;;;; to identify the underlying abstractions in our programs and
;;;;; to build upon them and generalize them to create more powerful abstractions.

;Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       (c))))
; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
(((double (double double)) inc) 5)
(((double (lambda (x) (double (double x)))) inc) 5)
(((lambda (y)
    ((lambda (x) (double (double x)))
     ((lambda (x) (double (double x)))
      y)))
  inc)
 5)
; apply inc 2*2*2*2 times.

; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Exercise 1.43
(define (repeated f n)
  (define (iter acc current)
    (if (= 0 current)
        acc
        (iter (compose f acc) (- current 1))))
  (iter f (- n 1)))