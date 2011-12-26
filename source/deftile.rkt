#lang racket
(define-values (prop:render-name has-render-name? render-name)
  (make-struct-type-property 'render-name))
(define-values (prop:solid? has-solid? solid?)
  (make-struct-type-property 'solid?))
(define-values (prop:deadly? has-deadly? deadly?)
  (make-struct-type-property 'deadly?))

(struct tile (room posn [on-use #:auto] [on-step #:auto])
  #:transparent #:mutable #:auto-value '()
  #:property prop:render-name 'tile
  #:property prop:solid?  #f
  #:property prop:deadly? #f)

(define (tile-deadly? t)
  (let ((d (deadly? t)))
    (cond [(boolean? d) d]
          [(number? d) (vector-ref (struct->vector t) d)]
          [else (printf "tile-deadly?: invalid value for deadly? in ~a.~ndeadly: ~a" 
                        t d) #t])))
(define (tile-solid? t)
  (let ((s (solid? t)))
    (cond [(boolean? s) s]
          [(number? s) (vector-ref (struct->vector t) s)]
          [else (printf "tile-deadly?: invalid value for deadly? in ~a.~ndeadly: ~a" 
                        t s) #t])))

(define (add-on-step! t fn)
  (let ((tos (tile-on-step t)))
    (set-tile-on-step! (cons fn tos))))

(define (add-on-use! t fn)
  (let ((tos (tile-on-step t)))
    (set-tile-on-step! (cons fn tos))))

(define (use-tile t)
  (let ((tr (tile-room t)))
    (for-each (λ (x) (x tr)) (tile-on-use t))))

(define (step-tile t)
  (let ((tr (tile-room t)))
    (for-each (λ (x) (x tr)) (tile-on-step t))))


(define-syntax deftile
  (syntax-rules ()
    [(_ (name super) fields is-solid? is-deadly?)
     (struct name super fields #:transparent #:mutable
       #:property prop:render-name (quote name)
       #:property prop:deadly? is-deadly?
       #:property prop:solid? is-solid?)]
    [(_ name-no-sup fields is-solid? is-deadly?)
     (deftile (name-no-sup tile) fields is-solid? is-deadly?)]
    [(_ name) (deftile name () #f #f)]
    [(_ name (fld ...)) (deftile name (fld ...) #f #f)]
    [(_ name is-solid?) (deftile name () is-solid? #f)]
    [(_ name (fld ...) is-solid?) (deftile name (fld ...) is-solid? #f)]
    [(_ name is-solid? is-deadly?) (deftile name () is-solid? is-deadly?)]))


(deftile grass)
(deftile floor)
(deftile screen #t)
(deftile dirt)
(deftile test)
(deftile sand)
(deftile teleporter)
(deftile wall #t)
(deftile lava #f #t)

(deftile switch (on?) #f)
(deftile (floor-2 switch))
(deftile (screen-1 switch) #t) 
(deftile (door switch) (open?) (struct-field-index open?))
