#lang racket/base
(require racket/gui/base
         racket/class
         "utils.rkt"
         "render.rkt"
         "game.rkt"
         "sound.rkt")
(define *game-name* "W")
(define *width*     800)
(define *height*    480)
(define *scale*     5)
(define *debug*     #f)



(define input-handler%
  (class object%
    (super-new)
    (struct keys ([up    #:auto] 
                  [down  #:auto] 
                  [left  #:auto] 
                  [right #:auto] 
                  [restart #:auto]
                  [pause #:auto] 
                  [godmode #:auto]
                  [debug #:auto]
                  [use   #:auto]) 
      #:auto-value #f
      #:transparent
      #:mutable)
    (define pressed (keys))
    (define (keys->list ks)
      (filter (λ (k) (not (void? k)))
              `(,(when (keys-up ks) 'up)
                ,(when (keys-down ks) 'down)
                ,(when (keys-left ks) 'left)
                ,(when (keys-right ks) 'right)
                ,(when (keys-pause ks) 'pause)
                ,(when (keys-godmode ks) 'godmode)
                ,(when (keys-restart ks) 'restart)
                ,(when (keys-debug ks) 'debug)
                ,(when (keys-use ks) 'use))))
    (define/public (active-keys)
      (keys->list pressed))
    (define/public (on-char ev)
      (let* ([p? (not (eq? 'release (send ev get-key-code)))]
             [kc (if p? (send ev get-key-code) (send ev get-key-release-code))])
         (case kc
           [(up #\w #\W)     (set-keys-up!    pressed p?)]
           [(down #\s #\S)   (set-keys-down!  pressed p?)]
           [(right #\d #\D)  (set-keys-right! pressed p?)]
           [(left #\a #\A)   (set-keys-left!  pressed p?)]
           [(space #\space)  (set-keys-use!   pressed p?)]
           [(#\g #\G)        (set-keys-godmode! pressed p?)]
           [(escape #\q #\Q) (set-keys-restart! pressed p?)]
           #;[(e #\e #\E)      (set-keys-debug! pressed p?)]
           #;[(#\i #\I)        (set-keys-pause! pressed p?)]
           )))))


(define cvs
  (class canvas%
    (super-new)
    (inherit get-dc get-client-size refresh)
    (send* (get-dc) 
      (set-scale *scale* *scale*)
      (set-background "black"))
    (define input-handler (make-object input-handler%))
    (define pause? #f)
    (define game (make-object game%))
    (define millis (current-inexact-milliseconds))
    (define tickmillis (current-inexact-milliseconds))
    (define frames 0)
    (define ticks 0)
    (define tick? #t)
    (define abort? #f)
    (define game-over? #f)
    (define running? #f)
    (define timer #f)
    (define/override (on-char ev)
      (send input-handler on-char ev)
      (when (or (and game-over? (member 'use (send input-handler active-keys)))
                (member 'restart (send input-handler active-keys)))
        (set! game-over? #f)
        (set! game (make-object game%)))
      
      (send input-handler on-char ev)
      (refresh))
    (define/public (run)
      (unless abort?
        (when (member 'abort (send input-handler active-keys)) 
          (set! abort? #t))
        (refresh)))
    (define/public (start)
      (unless running?
        (set! running? #t)
        (unless timer
          (let((that this))
            (set! timer (new timer% [interval 17] [notify-callback (λ _ (send that run))]))))
        ))
    (define/public (stop)
      (when running?
        (set! running? #f)
        (when timer
          (send timer stop))))
    (define/override (on-paint)
      (when (and *debug* (>= (- (current-inexact-milliseconds) millis) 3000.0))
        (printf "~a frames per second   ~a ticks per second~n" 
                (floor (/ frames 3)) (floor (/ ticks 3)))
        (set! millis (current-inexact-milliseconds))
        (set! frames 0)
        (set! ticks  0))
      (set! ticks (add1 ticks))
      (set! tickmillis (current-inexact-milliseconds))
      
      (unless game-over?
        (send game tick (send input-handler active-keys))
        (when (send game over?) 
          (play-effect 'game-over)
          (set! game-over? #t)))
      (let-values ([(w h) (get-client-size)])
        (render game (get-dc) w h))
      (play-effects (send game get-sounds))
      (set! frames (add1 frames)))
  
  ))

(define (make-solid-bitmap w h col)
  (let ((b (make-bitmap w h)))
    (send* (new bitmap-dc% [bitmap b])
      (set-background col)
      (clear))
    b))

(define (start)
  
  (define semaphore (make-semaphore 0))
  
  (define frame 
    (make-object
        (class frame%
          (define/augment (on-close)
            (semaphore-post semaphore)
            (inner (void) on-close))
          (super-new)) *game-name*))
  
  (define canvas (make-object cvs frame))
  
  (send* frame 
    (min-width *width*)
    (min-height *height*)
    (stretchable-height #f)
    (stretchable-width #f)
    (show #t))
  
  (when *debug*
    (let ((bblit (make-solid-bitmap 16 16 "blue"))
          (wblit (make-solid-bitmap 16 16 "white")))
    (register-collecting-blit canvas 2 2 16 16 bblit wblit)))

  (send canvas start)
  (void (yield semaphore))
  (send canvas stop))

(start)





