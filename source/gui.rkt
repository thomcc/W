#lang racket/base
(require racket/gui/base
         racket/class
         racket/set
         "utils.rkt"
         "render.rkt"
         "game.rkt"
         "sound.rkt"
         "core.rkt")
(provide w-canvas%)
(define timer-interval (/ 1 60))

(define input-handler%
  (class object%
    (super-new)
    
    (define pressed (seteq))
    
    (define/public (active-keys) pressed)
    
    (define/public (on-char ev)
      (let* ([press? (not (eq? 'release (send ev get-key-code)))]
             [kc (if press? (send ev get-key-code) (send ev get-key-release-code))])
        (when-let ((key (interpret-key kc)))
          (if press?
              (set! pressed (set-add pressed key))
              (set! pressed (set-remove pressed key))))))
    
    (define (interpret-key kc)
      (case kc
        [(up #\w #\W)     'up]
        [(down #\s #\S)   'down]
        [(right #\d #\D)  'right]
        [(left #\a #\A)   'left]
        [(space #\space)  'use]
        [(#\g #\G)        'godmode]
        [(escape #\q #\Q) 'restart]
        ))
    ))


(define w-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc get-client-size refresh get-parent)
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
    (define scale         *scale*)
    
    (define/override (on-size w h)
      (let* ((wsc (/ w PIX_WIDE))
             (hsc (/ h PIX_HIGH))
             (sc (min wsc hsc))
             (ww (* sc PIX_WIDE))
             (hh (* sc PIX_HIGH)))
        (set! scale sc)
        (send (get-dc) set-scale sc sc)
        ;(send (get-parent) resize ww hh)
        ))
        
    
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
          (set! timer 
                (new timer% [interval (inexact->exact 
                                       (floor (* 1000.0 timer-interval)))] 
                     [notify-callback 
                      (λ _ (send this run))])))))
    
    (define/public (stop)
      (when running?
        (set! running? #f)
        (when timer
          (send timer stop))))
    
    (define/override (on-paint)
      (when (>= (- (current-inexact-milliseconds) millis) 3000.0)
        (log-debug (format "~a fps~n" (floor* frames 3)))
        (set! millis (current-inexact-milliseconds))
        (set! frames 0))
      
      (unless game-over?
        (send game tick (send input-handler active-keys))
        (when (send game over?) 
          (play-effect 'game-over)
          (set! game-over? #t)))
      
      (let-values (((w h) (get-client-size)))
        (render game (get-dc) 
                (floor* w *scale*) 
                (floor* h *scale*)))
      
      (play-effects (send game get-sounds))
      (set! frames (add1 frames)))
      
  
  ))
