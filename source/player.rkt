#lang racket/base
(require racket/class "utils.rkt")
(provide player%)
(define player%
  (class object%
    (super-new)
    (init-field x y direction game)
    (field [step         0]
           [tilex       -1]
           [tiley       -1]
           [dead?       #f])
    
    (define motion 0.0) 
    ; private field for calculating the current part of the animation
    ; (should the player even know about this?)
    
    (define (on-move x y)
      (let ((tx (inexact->exact (floor (+ 0.5 x))))
            (ty (inexact->exact (floor (+ 0.5 y)))))
        (unless (and (= tx tilex) (= ty tiley))
          (set! tilex tx) 
          (set! tiley ty)
          (on-tile-change tilex tiley))))
    
    (on-move x y)

    ; constant
    (define speed            0.06)
    (define friction         0.10)
    (define hit-right        0.00)
    (define hit-left        -0.05)
    (define hit-foot         0.46)
    (define hit-head        -0.30)
    
    
    (define/public (get-loc) (cons tilex tiley))
    (define/public (get-dir) direction)
    
    (define/public (set-x nx) (set! x nx) (on-move x y))
    (define/public (set-y ny) (set! y ny) (on-move x y))
    (define/public (set-pos nx ny) (set! x nx) (set! y ny) (on-move x y))
    
    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-pos) (cons x y))
    
    (define/public (get-step) (modulo (inexact->exact (floor step)) 4))
    (define/public (step!) (set! step (modulo (add1 step) 4)))
    
    (define (maybe-step) (when (> motion 0.5) (step!) (set! motion 0.0)))
    (define (reset-step) (set! step 0))
    (define (die) 
      (unless (send game god-mode?)
        (set! dead? #t) (on-death)))
    
    (define (reorient u d l r)
      (let ((last direction))
        (unless (or (not (or u d l r)) (and u d l r))
          (when (not (eq? l r))
            (set! direction (if r 'right 'left)))
          (when (not (eq? u d))
            (set! direction (if d 'down 'up))))
        (if (eq? direction last) 
            (maybe-step)
            (reset-step))))
    
    (define (on-tile-change x y) (send game tile-changed x y))
    (define (on-death) (send game lose))
    
    (define move
      (let ((xa 0.0) (ya 0.0))
        (define (do-move)
          (let ((ys (floor* (add1 (abs (* xa 100)))))
                (xs (floor* (add1 (abs (* ya 100))))))
            (let loop ([i xs])
              (when (> i 0)
                (let ((nx (+ (get-x) (* xa (/ i xs)))))
                  (if (check-spot nx y) 
                      (set! x nx)
                      (begin (set! xa 0.0) 
                             (loop (sub1 i)))))))
            (let loop ([i ys])
              (when (> i 0)
                (let* ((ny (+ y (* ya (/ i ys)))))
                  (if (check-spot x ny) 
                      (set! y ny)
                      (begin (set! ya 0.0) 
                             (loop (sub1 i)))))))))
        (λ (xp yp)
          (dec! xa (* xp speed))
          (dec! ya (* yp speed))
          (do-move)
          (set! xa (* xa friction))
          (set! ya (* ya friction))
          (on-move x y))))
    
    (define/public (tick u d l r)
      (if (or (not (or u d l r)) (and u d l r)) 
          (set! step 0)
          (let* ([xm (+ (if l 1.0 0.0) (if r -1.0 0.0))]
                 [ym (+ (if u 1.0 0.0) (if d -1.0 0.0))]
                 [dist^2 (+ (* xm xm) (* ym ym))]
                 [dist (if (> 0.0 dist^2) (sqrt dist^2) 1.0)]
                 [x-norm (/ xm dist)] [y-norm (/ ym dist)])
            (inc! motion (* speed (sqrt (+ (* x-norm x-norm) (* y-norm y-norm)))))
            (reorient u d l r)
            (move x-norm y-norm))))
    
    (define/public (check-spot [xx x] [yy y])
      (let ((x0 (inexact->exact (floor (+ xx 0.5 (- hit-left)))))
            (x1 (inexact->exact (floor (+ xx 0.5    hit-right))))
            (y0 (inexact->exact (floor (+ yy 0.5 (- hit-head)))))
            (y1 (inexact->exact (floor (+ yy 0.5    hit-foot)))))
        (when (check-deadly x0 y0 x1 y1)
          (die))
        (and (not dead?)
             (not (send game blocked? x0 y0))
             (not (send game blocked? x1 y0))
             (not (send game blocked? x0 y1))
             (not (send game blocked? x1 y1)))
        ))
    (define (check-deadly x0 y0 x1 y1)
      (or (send game deadly? x0 y0)
          (send game deadly? x1 y0)
          (send game deadly? x0 y1)
          (send game deadly? x1 y1)))
            
    (define/public (get-draw-info) (values (exact->inexact x) (exact->inexact y) direction step))
    
    ))
