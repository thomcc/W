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


