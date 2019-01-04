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