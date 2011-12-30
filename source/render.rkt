#lang racket/gui

(require "utils.rkt"
         racket/runtime-path
         racket/require
         racket/flonum
         racket/fixnum
         #;(filtered-in
          (λ (name) (regexp-replace #rx"unsafe-" name ""))
          racket/unsafe/ops))
(provide render)
(define square-hash
  #hasheq((player-r        . (0 . 2))
          (player-d        . (0 . 3))
          (player-u        . (0 . 4))
          (player-l        . (0 . 5))
          (test            . (0 . 0))
          (boulder         . (1 . 0))
          (grass           . (2 . 0))
          (dirt            . (3 . 0))
          (sand            . (4 . 0))
          (screen-on       . (5 . 0))
          (screen-off      . (6 . 0))
          (floor-on        . (7 . 0))
          (yes-f           . (7 . 0))
          (floor-off       . (8 . 0))
          (no-f            . (8 . 0))
          (lava            . (0 . 1))
          (teleport        . (1 . 1))
          (flor            . (2 . 1))
          (wall            . (3 . 1))
          (door-lr-open    . (4 . 1))
          (door-lr-close   . (6 . 1))
          (door-ud-open    . (4 . 2))
          (door-ud-close   . (4 . 4))))
(define anim-hash
  #hasheq((right      . #((0 . 2) (1 . 2) (2 . 2) (3 . 2)))
          (down       . #((0 . 3) (1 . 3) (2 . 3) (3 . 3)))
          (up         . #((0 . 4) (1 . 4) (2 . 4) (3 . 4)))
          (left       . #((0 . 5) (1 . 5) (2 . 5) (3 . 5)))
          (zap        . #((0 . 6) (1 . 6) (2 . 6) (3 . 6) 
                                  (4 . 6) (5 . 6) (6 . 6)))))
(define square-size 16)
(define square-size.0 16.0)
(define squares
  (for/hasheq (((k v) (in-hash square-hash)))
    (values k (cons (fx* square-size (car v))
                    (fx* square-size (cdr v))))))

(define-runtime-path image-location "../res/img.png")
(define the-bitmap (read-bitmap image-location 'png/alpha))
(define anim-size 16)

(define animations
  (for/hasheq (((k v) (in-hash anim-hash)))
    (values k (for/vector ((c (in-vector v)))
                (cons (fx* anim-size (car c)) 
                      (fx* anim-size (cdr c)))))))



(define (draw-animation which step x y dc)
  (let ((pt (vector-ref (hash-ref animations which) step)))
    (send dc draw-bitmap-section the-bitmap x y
          (car pt) (cdr pt) anim-size anim-size)))

(define (draw-game-over xo yo dc)
  (let-values (((sx sy) (send dc get-scale)))
    (send dc set-scale 8 8)
    (send dc draw-bitmap-section the-bitmap 
          (fx+ xo 18) (fx+ yo 12) (fx* 16 7) 16 64 16)
    (send dc draw-bitmap-section the-bitmap 
          (fx+ xo 18) (fx+ yo 36) (fx* 16 12) 16 64 16)
    (send dc set-scale sx sy)))

(define (render game dc w h)
  (let ((level (send game get-level))
        (dyn   (send game get-dynamic)))
    (send dc clear)
    (let* ((ghei (fx* square-size (vector-length level)))
           (gwid (fx* square-size (vector-length (vector-ref level 0))))
           (wdif (fx- h ghei))
           (hdif (fx- w ghei)))
      (when (or (negative? wdif) (negative? hdif))
        (printf "uh oh! screen is too small: game-width: ~a game-height: ~a w: ~a h: ~a~n"
              ghei gwid w h))
      (let ((x-offset (fxmin (fxquotient wdif 2) 0))
            (y-offset (fxmin (fxquotient hdif 2) 0)))
        
        ;; draw the tiles
        (for ([row  (in-vector level)] 
              [yi   (in-range 0 (fx* square-size (vector-length level)) square-size)] 
              #:when #t
              [spot (in-vector row)]   
              [xi (in-range 0 (fx* square-size (vector-length row)) square-size)])
          (let ((pt (hash-ref
                     squares spot
                     (λ (e) (printf "draws: not-found: ~a~n" e) '(0 . 0)))))
            (send dc draw-bitmap-section the-bitmap 
                  (fx+ x-offset xi) (fx+ y-offset yi)
                  (car pt)        (cdr pt) 
                  square-size     square-size)))
        
       ;; draw the animations 
        (for ([e (in-list dyn)])
          (let-values (((x y name step) (send e get-draw-info)))
            (draw-animation name step
                             (fx+ x-offset (fl->fx (flfloor (fl* square-size.0 x))))
                             (fx+ y-offset (fl->fx (flfloor (fl* square-size.0 y))))
                             dc)))
        (when (send game over?) (draw-game-over x-offset y-offset dc))))))



