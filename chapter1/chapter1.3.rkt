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
