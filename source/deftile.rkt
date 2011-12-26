#lang racket
(struct tile (room posn [on-use #:auto] [on-step #:auto])
  #:transparent #:mutable #:auto-value '()
  #:property prop:render-name 'tile
  #:property prop:solid?  #f
  #:property prop:deadly? #f)


