#lang racket/base
(require racket/class "utils.rkt" "tile.rkt" "level.rkt")
(provide game% player%)


(define player%
  (class object%
    (super-new)
    (init xx yy dir owner)
    (field [x           -1.0]
           [y           -1.0]
           [direction   'down]
           [step         0]
           [tilex       -1]
           [tiley       -1]
           [dead?       #f]
           [game        #f])
    
    (define (on-move x y)
      (let ((tx (inexact->exact (floor (+ 0.5 x))))
            (ty (inexact->exact (floor (+ 0.5 y)))))
        (unless (and (= tx tilex) (= ty tiley))
          (set! tilex tx) 
          (set! tiley ty)
          (on-tile-change tilex tiley))))
    
    (set! game       owner)
    (set! direction  dir)
    (set! x          xx)
    (set! y          yy)
    
    (on-move x y)

    ; constant
    (define speed     0.06)
    (define friction  0.10)
    (define hit-right       0.00)
    (define hit-left       -0.05)
    (define hit-foot        0.40)
    (define hit-head        0.00)
    
    (define motion 0.0)
    
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
        (when (or (send game deadly? x0 y0)
                  (send game deadly? x1 y0)
                  (send game deadly? x0 y1)
                  (send game deadly? x1 y1))
          (die))
        (and (not dead?)
             (not (send game blocked? x0 y0))
             (not (send game blocked? x1 y0))
             (not (send game blocked? x0 y1))
             (not (send game blocked? x1 y1)))
        ))
    
            
    (define/public (get-draw-info) (values x y direction step))
    
    ))



(define zap
  (class object%
    (super-new)
    (init x y)
    (define step 0)
    (define xx x)
    (define yy y)
    (define/public (get-draw-info) (values xx yy 'zap step))
    (define/public (step!) (set! step (add1 step)))))

(define game%
  (class object%
    (super-new)
    (define player (make-object player% -1 -1 'right this))
    (define entities (list player))
    (define teleporting? #f)
    (define teleport-count 0)
    (define teleport-to #f)
    (define game-over? #f)
    
    (define god? #f)
    (define sounds '())
    (define current-level 'none)
    
    
    
    (define/public (get-dynamic) entities)
    (define/public (get-sounds) sounds)
    
    (define/public (deadly? x y)
      (with-handlers ([exn:fail? (λ _ #f)])
        (if (not (in-range x y))
            #f
            (tile-deadly? (vref (level-data current-level) y x)))))
    
    (define/public (blocked? x y)
      (with-handlers ([exn:fail? (λ _ #f)]) ; too close to end to do this better
        (if (not (in-range x y)) 
            #f
            (tile-solid? (vref (level-data current-level) y x)))))
    (define/public (god-mode?) god?)
    (define/public (set-level which-level [init? #f])
      (set! current-level (which-level))
      (if init?
          (push! sounds 'game-start)
          (push! sounds 'solve))
      (send player set-pos (car (level-spawn current-level)) (cdr (level-spawn current-level))))
    (define/public (get-tile x y) (vector-ref (vector-ref (level-data current-level) y) x))
    (define/public (get-entities) entities)
    (define/public (get-level) (make-exportable current-level))
    (define/public (get-player) player)
    (define/public (over?) game-over?)
    (define/public (lose) (set! game-over? #t))
    
    (define (check-flags keys)
      (for ([k keys])
        (case k
          [(godmode) (set! god? #t)])))
    
    (define (clear-sounds) (set! sounds '()))
    
    (define/public (tick keys)
      (clear-sounds)
      (when (eq? current-level 'none)
        (set-level level:start #t))
      (check-flags keys)
      (do-tick keys))
    
    (define/public (tile-changed tx ty)
      (let ((change-level? (alist-get (cons tx ty) (level-exits current-level))))
        (when change-level? (set-level change-level?)))
      (send player check-spot))
    
    (define (usable? p)
      (if (not (in-range (car p) (cdr p))) 
          #f 
          (tile-usable? (vref (level-data current-level) (cdr p) (car p)))))
    
    (define (use) 
      (let* ([p (send player get-loc)]
             [d (send player get-dir)]
             [looking (pt+ (get-delta d) p)]
             [active (cond [(usable? p)
                            (vref (level-data current-level) (cdr p) (car p))]
                           [(usable? looking)
                            (vref (level-data current-level) 
                                  (cdr looking) (car looking))]
                           [else #f])])
        (when active
          (use-tile active))))
    
    (define (in-range? x y)
      (and (>= x 0) (>= y 0)
           (< y (vector-length (level-data current-level)))
           (< x (vector-length (vector-ref (level-data current-level) 0)))))
    
    (define (screen-alert t v)
      (cond [(procedure? t)
             (t (level-data current-level) this)]
            [(screen? t)
             (set-screen-on?! t v)]
            [(door? t)
             (set-door-locked?! t (not (door-locked? t)))]))
    
    (define/public (teleport-player tile)
      (unless teleporting?
        (let ((t (send player get-loc)))
          (set! teleporting? #t)
          (set! teleport-count 0)
          (set! teleport-to tile)
          (push! entities (make-object zap (car t) (cdr t))))))
    
    (define (use-tile t)
      (cond
        [(screen? t) 
         (push! sounds 'screen)
         (use-screen t)
         (screen-alert (screen-controls t) (screen-on? t))]
        [(teleport? t) 
         (push! sounds 'zap)
         (teleport-player (teleport-dest t))]
        [(door? t) 
         (if (use-door t)
             (push! sounds 'door)
             (push! sounds 'locked))]))
    
    
    (define (continue-teleporting)
      (cond [(>= teleport-count 6);done teleporting
             (set! teleporting? #f)
             (send player set-pos 
                   (tile-x teleport-to)
                   (tile-y teleport-to))
             (set! teleport-count 0)
             (set! entities (list player))]
            [(= teleport-count 4) ; player hidden, stop drawing him.
             (set! entities (list (car entities)))
             (set! teleport-count (add1 teleport-count))
             (send (car entities) step!)]
            [else (send (car entities) step!)
                  (set! teleport-count (add1 teleport-count))]))
    (define do-tick 
      (let ((using? #f)) ; prevents use from repeatedly firing
        (λ (keys)
          (cond [teleporting? (continue-teleporting)]
                [(member 'use keys)
                 (unless using? 
                   (set! using? #t) 
                   (use))]
                [else (set! using? #f)
                      (send player tick 
                            (member 'up keys) 
                            (member 'down keys)
                            (member 'left keys) 
                            (member 'right keys))]))))
    ))
