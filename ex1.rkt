#lang racket

;; The empty lazy list value (a singleton datatype)
(define empty-lzl '())

;; Purpose: Value constructor for non-empty lazy-list values
;; Type: [T * [Empty -> LZL(T)] -> LZT(T)]
(define cons-lzl cons)

;; Accessors
;; Type: [LZL(T) -> T]
;; Precondition: Input is non-empty
(define head car)

;; Type: [LZL(T) -> LZL(T)]
;; Precondition: Input is non-empty
;; Note that this *executes* the continuation 
(define tail
  (lambda (lzl)
  ((cdr lzl))))
  
;; Type predicate
(define empty-lzl? empty?)

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

; Signature: nth(lz-lst,n)
;; Type: [LzL*Number -> T]
;; Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (sub1 n)))))

(define facts-gen
  (lambda ()
    (letrec ((loop (lambda (n fact-n)
                     (cons-lzl fact-n 
                               (lambda () (loop (+ n 1) 
                                                (* (+ n 1) fact-n)))))))
      (loop 1 1))))


;; Signature: lzl-map(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define lzl-map
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (lzl-map f (tail lzl)))))))

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))
(define ints (integers-from 0))
;(take (lzl-map (lambda (x) (* x x)) ints) 5)

;; Signature: lz-lst-filter(p,lz)
;; Type: [[T1 -> Boolean] * Lzl(T1) -> LzL(T1)]
(define lzl-filter
  (lambda (p lzl)
    (cond ((empty-lzl? lzl) lzl)
          ((p (head lzl)) (cons-lzl (head lzl) 
                                    (lambda () (lzl-filter p (tail lzl)))))
          (else (lzl-filter p (tail lzl))))))

(define divisible?
  (lambda (x y)
    (= (remainder x y) 0)))

;; Signature: sieve(lzl)
;; Type: [Lzl(Number) -> Lzl(Number)]
(define sieve
  (lambda (lzl)
    (cons-lzl (head lzl)
              (lambda ()
                (sieve (lzl-filter (lambda (x) (not (divisible? x (head lzl))))
                                   (tail lzl)))))))
                                   
(define primes1 (sieve (integers-from 2)))
;(take primes1 7)

;; Signature: integers-iterate(f,n)
;; Type: [[Number -> Number] * Number -> Lzl(Number)]
(define integers-iterate
  (lambda (f n)
    (cons-lzl n (lambda () (integers-iterate f (f n))))))

;(take (integers-iterate (lambda (k) (+ k 1)) 3) 7)
;; --> ’(3 4 5 6 7 8 9)

;(take (integers-iterate (lambda (k) (* k 2)) 3) 7)
;; ’(3 6 12 24 48 96 192)

;(take (integers-iterate (lambda (k) k) 3) 7)
;; --> ’(3 3 3 3 3 3 3)

;; Primes – First definition
(define primes
  (cons-lzl 2 (lambda () (lzl-filter prime? (integers-from 3)))))

(define prime?
  (lambda (n)
    (letrec ((iter (lambda (lz)
                     (cond ((> (sqr (head lz)) n) #t)
                           ((divisible? n (head lz)) #f)
                           (else (iter (tail lz)))))))
      (iter primes))))


;; --> ’(2 3 5 7 11 13)

(define average
  (lambda (x y) (/ (+ x y) 2)))

(define abs (lambda (x) (if (< x 0) (- x) x)))

(define square (lambda (x) (* x x)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define sqrt-lzl (lambda (x init) (sqrt-lzl-iter x init)))

(define accuary
  (lambda (guess x)
    (abs (- (square guess) x))))

(define good-enough?
  (lambda (guess x epsilon)
    (< (abs (- (square guess) x)) epsilon)))


(define sqrt-lzl-iter
  (lambda (x guess)
    (cons-lzl (cons guess (accuary guess x))
              (lambda ()
                (sqrt-lzl-iter x (improve guess x))))))

(define find-first
  (lambda (lzlst p)
    (if (empty-lzl? lzlst)
        'fail
        (if (p (head lzlst))
            (head lzlst)
            (find-first (tail lzlst) p)))))

(define sqrt2
  (lambda (x init epsilon)
    (car (find-first (sqrt-lzl x init)
                (lambda (guess) (good-enough? (car guess) x epsilon))))))




(display "1.2.a\n")
(take (sqrt-lzl 2 1) 5)
(display "1.2.b\n")
(find-first (integers-from 1) (lambda (x) (> x 10)))
(find-first (cons-lzl 1 (lambda() (cons-lzl 2 (lambda () '())))) (lambda (x) (> x 10)))
(display "1.2.c\n")
(sqrt2 2 1 0.0001)
 

