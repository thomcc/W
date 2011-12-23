#lang racket/base
(require racket/gui/base
         racket/class
         racket/set
         "utils.rkt"
         "render.rkt"
         "game.rkt"
         "sound.rkt"
         "params.rkt")
(provide cvs)
(define input-handler%
  (class object%
    (super-new)
    (struct keys ([up    #:auto] 
                  [down  #:auto] 
                  [left  #:auto] 
                  [right #:auto] 
                  [restart #:auto]
                  [godmode #:auto]
                  [use   #:auto]) 
      #:auto-value #f
      #:transparent
      #:mutable)
    (define pressed (keys))
    (define (keys->set ks)
      (set-remove (seteq (when (keys-up ks) 'up)
                         (when (keys-down ks) 'down)
                         (when (keys-left ks) 'left)
                         (when (keys-right ks) 'right)
                         (when (and (*debug*) (keys-godmode ks)) 'godmode)
                         (when (keys-restart ks) 'restart)
                         (when (keys-use ks) 'use))
              (void)))
    (define/public (active-keys)
      (keys->set pressed))
    (define/public (on-char ev)
      (let* ([p? (not (eq? 'release (send ev get-key-code)))]
             [kc (if p? (send ev get-key-code) (send ev get-key-release-code))])
         (case kc
           [(up #\w #\W)     (set-keys-up!      pressed p?)]
           [(down #\s #\S)   (set-keys-down!    pressed p?)]
           [(right #\d #\D)  (set-keys-right!   pressed p?)]
           [(left #\a #\A)   (set-keys-left!    pressed p?)]
           [(space #\space)  (set-keys-use!     pressed p?)]
           [(#\g #\G)        (set-keys-godmode! pressed p?)]
           [(escape #\q #\Q) (set-keys-restart! pressed p?)]
           )))))


(define cvs
  (class canvas%
    (super-new)
    (inherit get-dc get-client-size refresh)
    (send* (get-dc) 
      (set-scale *scale* *scale*)
      (set-background "black"))
    
    (define input-handler (make-object input-handler%))
    (define game          (make-object game%))
    (define millis        (current-inexact-milliseconds))
    (define frames        0)
    (define game-over?    #f)
    (define running?      #f)
    (define timer         #f)
    
    
    (define/override (on-char ev)
      (send input-handler on-char ev)
      (when (or (and game-over? 
                     (set-member? (send input-handler active-keys) 'use))
                (set-member? (send input-handler active-keys) 'restart))
        (set! game-over? #f)
        (set! game (make-object game%))))
    
    (define/public (run) 
     
      (refresh)
      )
    
    (define/public (start)
      (unless running?
        (set! running? #t)
        (unless timer
          (set! timer (new timer% [interval 17] 
                           [notify-callback 
                            (λ _ (send this run))])))))
    
    (define/public (stop)
      (when running?
        (set! running? #f)
        (when timer
          (send timer stop))))
    
    (define/override (on-paint)
      (when (and (*debug*)
                 ((- (current-inexact-milliseconds) millis) . >= . 3000.0))
        (printf "~a fps~n" (floor* frames 3))
        (set! millis (current-inexact-milliseconds))
        (set! frames 0))
      
      (unless game-over?
        (send game tick (send input-handler active-keys))
        (when (send game over?) 
          (play-effect 'game-over)
          (set! game-over? #t)))
      
      (render game (get-dc))
      
      (play-effects (send game get-sounds))
      (set! frames (add1 frames)))
      
  
  ))
