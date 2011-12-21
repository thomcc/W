#lang racket/base

(provide (all-defined-out))


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
(define (pt->values p) ; ugh i should probably make these structs
  (values (car p) (cdr p)))
(define values->pt cons) ; for symmetry, even if it is dumb
(define (pt-mod p m)
  (cons (modulo (car p) (car m))
        (modulo (cdr p) (cdr m))))
(define-syntax-rule (inc! x v) (set! x (+ x v)))
(define-syntax-rule (dec! x v) (set! x (- x v)))

(define (floor* x [d 1]) (inexact->exact (floor (/ x d))))

(define (round* x) (inexact->exact (round x)))

(define (random-element lst) 
  (list-ref lst (random (length lst))))
(define (clamp x min max)
  (cond [(< x min) min] [(> x max) max] [#t x]))


(define (exit-dir x y)
  ;(let ([x0? (<= x 0)] [x9? (>= x 9)]
  ;      [y0? (<= y 0)] [y5? (>= y 5)])
  ;  (cond [(and x0? y0?)  
  (cond [(= x 0) 'left] [(>= x 9) 'right] [(= y 0) 'up] [(>= y 5) 'down] [else (printf "dont know what to do for ~a, ~a~n" x y) 'up]))

(define (get-delta dir) 
  (alist-get dir '((up    . (0 . -1)) 
                    (down  . (0 . +1))
                    (left  . (-1 . 0)) 
                    (right . (+1 . 0)))))

(define (opposite dir)
  (case dir 
    [(up)    'down]
    [(down)  'up]
    [(left)  'right]
    [(right) 'left]
    [else (error 'opposite "not a direction! ~a" dir)]))

(define (wrap-around x y [xoff 0] [yoff 0])
  (define-values ( xmax  ymax) (values 10 6))
  (let ((d (get-delta (exit-dir x y) #;(opposite (exit-dir x y)))))
    (pt->values (pt-mod (pt+ (cons x y) d) (cons xmax ymax)))))