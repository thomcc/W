#lang racket/gui
(require "utils.rkt")
(require racket/runtime-path)
(provide render)

(define-runtime-path image-location "../res/img.png")
(define-values (draw-animation draw-static draw-game-over)
  (let ()
    (define square-size 16)
    ;(define image-location "res/img.png")
    (define squares
      '((player-r        . (0 . 2))
        (player-d        . (0 . 3))
        (player-u        . (0 . 4))
        (player-l        . (0 . 5))
        (test            . (0 . 0))
        (boulder         . (1 . 0))
        (grass           . (2 . 0))
        (dirt            . (3 . 0))
        (magic           . (4 . 0))
        (screen-on       . (5 . 0))
        (screen-off      . (6 . 0))
        (lava            . (0 . 1))
        (teleport        . (1 . 1))
        (flor            . (2 . 1))
        (wall            . (3 . 1))
        (door-lr-open    . (4 . 1))
        (door-lr-close   . (6 . 1))
        (door-ud-open    . (4 . 2))
        (door-ud-close   . (4 . 4))))
    
    (define animations
      '((right      . #((0 . 2) (1 . 2) (2 . 2) (3 . 2)))
        (down       . #((0 . 3) (1 . 3) (2 . 3) (3 . 3)))
        (up         . #((0 . 4) (1 . 4) (2 . 4) (3 . 4)))
        (left       . #((0 . 5) (1 . 5) (2 . 5) (3 . 5)))
        (zap        . #((0 . 6) (1 . 6) (2 . 6) (3 . 6) (4 . 6) (5 . 6) (6 . 6)))))
    ;(define the-bitmap the-img)
    (define the-bitmap (read-bitmap image-location 'png/alpha))
    
    (define (draw-a which step x y dc)
      (let ((pt (vector-ref (alist-get which animations) step)))
        (send dc draw-bitmap-section the-bitmap x y
              (* 16 (car pt)) (* 16 (cdr pt)) 16 16)))
    
    (define (draw-s which x y dc)
      (let ((pt (alist-get 
                 which squares 
                 (λ (e) (printf "fuck ds: ~a~n" e) '(0 . 0)))))
        (send dc draw-bitmap-section the-bitmap x y
              (* 16 (car pt)) (* 16 (cdr pt)) 16 16)))
    (define (draw-go dc)
      (let-values (((sx sy) (send dc get-scale)))
        (send dc set-scale 8 8)
        (send dc draw-bitmap-section the-bitmap 18 12 (* 16 7) 16 64 16)
        (send dc draw-bitmap-section the-bitmap 18 36 (* 16 12) 16 64 16)
        (send dc set-scale sx sy)))
    (values draw-a draw-s draw-go)
    ))

(define (render game dc w h)
  (let ((level (send game get-level))
        (dyn   (send game get-dynamic)))
    (send dc clear)
    (for ([row  (in-vector level)] 
          [yi   (in-range 0 w  16)] 
          #:when #t
          [spot (in-vector row)]   
          [xi (in-range 0 h 16)])
      (draw-static spot xi yi dc)
      )
    (for ([e (in-list dyn)])
      (let-values (((x y name step) (send e get-draw-info)))
        (draw-animation name step 
                        (inexact->exact (* 16.0 x)) 
                        (inexact->exact (* 16.0 y))
                        dc))))  
  (when (send game over?) (draw-game-over dc)))
                                               



