#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(struct tile (x y deadly? solid? usable?) #:transparent #:mutable)

(define set-tile-pos! 
  (case-lambda 
    [(tile p) (set-tile-pos! tile (car p) (cdr p))]
    [(tile x y) (set-tile-x! tile x) (set-tile-y! tile y)]))


(define (tile-pos tile)
  (cons (tile-x tile)
        (tile-y tile)))

(define (tile-active? tile)
  (and (> 0 (tile-x tile))
       (> 0 (tile-y tile))))

(define (deactivate-tile! tile)
  (set-tile-pos! tile -1 -1))

(struct grass tile () #:transparent #:mutable)
(struct flor tile  () #:transparent #:mutable)
(struct yes-f tile () #:transparent #:mutable);; todo
(struct no-f tile  () #:transparent #:mutable);; these, but better
(struct dirt tile  () #:transparent #:mutable)
(struct lava tile  () #:transparent #:mutable)
(struct wall tile  () #:transparent #:mutable)
(struct test tile  () #:transparent #:mutable)
(struct sand tile  () #:transparent #:mutable)

(struct screen   tile (on? controls) #:transparent #:mutable)
(struct teleport tile (dest)     #:transparent #:mutable)
(struct door tile (dir locked?)  #:transparent #:mutable) ; dir is either 'ud or 'lr
(define (init-plain ctor)
  (λ ([p (cons -1 -1)]) (ctor (car p) (cdr p) #f #f #f)))
(define init-grass (init-plain grass))
(define init-flor  (init-plain flor))
(define init-sand  (init-plain sand))
(define init-dirt  (init-plain dirt))
(define init-test  (init-plain test))
(define init-yes-f (init-plain yes-f))
(define init-no-f  (init-plain no-f))

;(define (init-grass [p (cons -1 -1)]) (grass (car p) (cdr p) #f #f #f))
;(define (init-flor  [p (cons -1 -1)]) (flor (car p) (cdr p) #f #f #f))

;(define (init-flor  [p (cons -1 -1)]) (flor (car p) (cdr p) #f #f #f))
;(define (init-dirt  [p (cons -1 -1)]) (dirt  (car p) (cdr p) #f #f #f))
(define (init-lava  [p (cons -1 -1)]) (lava  (car p) (cdr p) #t #f #f))
(define (init-wall  [p (cons -1 -1)]) (wall  (car p) (cdr p) #f #t #f))
;(define (init-test  [p (cons -1 -1)]) (test  (car p) (cdr p) #f #f #f))
;(define (init-sand  [p (cons -1 -1)]) (sand  (car p) (cdr p) #f #f #f))
(define (init-teleport [p (cons -1 -1)]) (teleport (car p) (cdr p) #f #f #t 'none))
(define (init-screen on? [p (cons -1 -1)]) (screen (car p) (cdr p) #f #t #t on? 'none))
(define (init-door dir [p (cons -1 -1)] [open? #f] [locked? #f])
  (door  (car p) (cdr p) #f (not open?) #t dir locked?))

(define (use-screen s)
  (if (screen-on? s) 
      (screen-off s)
      (screen-on s)))


(define (screen-off s)
  (set-screen-on?! s #f))
(define (screen-on s)
  (set-screen-on?! s #t))

(define (use-door d)
  (let ((open? (door-open? d)))
    (if open? 
        (open-door d) 
        (close-door d))
    (not (boolean=? open? (door-open? d))))) ; return true if it changed state.

(define (open-door d)
  (unless (door-locked? d)
    (set-tile-solid?! d #f)))

(define (door-open? d)
  (tile-solid? d))

(define (close-door d)
  (when (door-locked? d) 
    (set-door-locked?! d #f))
  (set-tile-solid?! d #t))

(define (struct-string s)
  (cadr
   (regexp-match 
    #rx"^struct:(.*)$"
    (symbol->string (vector-ref (struct->vector s) 0)))))

(define (struct-name s)
  (string->symbol (struct-string s)))


(define (make-teleport-pair)
  (letrec ([ta (teleport -1 -1 #f #f #t tb)]
           [tb (teleport -1 -1 #f #f #t ta)])
    (values ta tb)))

(define (place-tile tile level x y)
  (when (tile? (vref level y x))
    (deactivate-tile! (vref level y x)))
  (set-tile-pos! tile x y)
  (vset! level tile y x))

(define (create-and-place-teleport-pairs tele-as tele-bs level) ; list of posns, should return list of teleports
  (unless (= (length tele-as) (length tele-bs))
    (error 'place-teleports "length of as and bs should be the same, given ~a and ~a" tele-as tele-bs))
  (map 
   (λ (a b) 
     (let-values ([(ta tb) (make-teleport-pair)])
       (place-tile ta level (car a) (cdr a))
       (place-tile tb level (car b) (cdr b))
       (cons ta tb)))
   tele-as tele-bs))



(define (create-and-place-teleport-list ts [cycle? #f])
  (let ((first-tele (init-teleport (car ts))))
    (let loop ([posns (cdr ts)] [teles `(,first-tele)])
      (cond [(null? posns) (when cycle? (set-teleport-dest! (car teles) first-tele)) teles]
            [else (let ([this-tele (init-teleport (car posns))] [last-tele (car teles)])
                    (set-teleport-dest! last-tele this-tele)
                    (loop (cdr posns) (cons this-tele teles)))]))))

(define (use-teleport t) ; returns new location, or #f to indicate that nothing should happen
  (and (tile-active? t) 
       (let ((td (teleport-dest t)))
         (if (eq? td 'none) #f (tile-pos td)))))
  