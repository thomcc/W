#lang racket/base
(require racket/class "utils.rkt" "tile.rkt" "level.rkt" "player.rkt")
(provide game%)






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
      (cond [(list? (level-exits current-level))
             (let ((change-level? (alist-get (cons tx ty) (level-exits current-level))))
               (when change-level? (set-level change-level?)))]
            [(symbol? (level-exits current-level))
             (case (level-exits current-level)
               [(wrap-around) (when (or (= tx -1) (= ty -1) (= ty 6) (= tx 10))
                                (call-with-values (λ () (wrap-around (clamp tx 0 9) (clamp ty 0 5)))
                                                  (λ (x y) (send player set-pos x y))))])])
      
 
      (send player check-spot))
    
    (define (usable? p)
      (if (not (in-range? (car p) (cdr p))) 
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
