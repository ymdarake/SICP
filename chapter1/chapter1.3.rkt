#lang racket

;;Procedures As Arguments

;ex. sigma notation
(define (sum f start next end)
  (if (> start end)
      0
      (+ (f start)
         (sum f (next start) next end))))

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

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