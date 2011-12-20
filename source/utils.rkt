#lang racket/base

(provide (all-defined-out))

(define (get-delta dir) 
  (cdr (assoc dir '((up    . (0 . -1)) 
                    (down  . (0 . +1))
                    (left  . (-1 . 0)) 
                    (right . (+1 . 0))))))
#;(define (replace-all list replacements)
  (map (λ (e) (let ((a (assoc e replacements)))
                (if a (cdr a) e))) list))
(define (ensure-list x) (if (list? x) x (list x)))
(define (alist-get x alist [otherwise (λ _ #f)])
  (let ((e (assoc x alist))) (if e (cdr e) (otherwise x))))
(define (pt+ p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(define vref
  (case-lambda
    ((v x) (vector-ref v x))
    ((v y x) (vector-ref (vector-ref v y) x))))

(define vset! 
  (case-lambda 
    ((v x val) (vector-set! v x val))
    ((v y x val) (vector-set! (vector-ref v y) x val))))

(define-syntax-rule (push! l x)
  (begin
    (set! l (cons x l))
    l))

(define-syntax-rule (pop! l)
  (let ((v (car l)))
    (set! l (cdr l))
    v))
(define (random-boolean [one-in-chance 2])
  (zero? (random one-in-chance)))
(define-syntax-rule (remove-from-list! l item)
  (begin 
    (set! l (remove item l))
    l))
(define (!= . args)
  (not (apply = args)))
(define (pt=? a b)
  (and (= (car a) (car b))
       (= (cdr a) (cdr b))))
(define-syntax-rule (inc! x v) (set! x (+ x v)))
(define-syntax-rule (dec! x v) (set! x (- x v)))

(define (floor* x [d 1]) (inexact->exact (floor (/ x d))))

(define (round* x) (inexact->exact (round x)))

(define (random-element lst) 
  (list-ref lst (random (length lst))))



