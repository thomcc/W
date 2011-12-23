#lang racket/base
(require racket/gui/base
         racket/class
         "gui.rkt"
         "utils.rkt"
         "params.rkt")


(define (make-solid-bitmap w h col)
  (let ((b (make-bitmap w h)))
    (send* (new bitmap-dc% [bitmap b])
      (set-background col)
      (clear))
    b))

(define (main)
  (*debug* #t)
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

(main)





