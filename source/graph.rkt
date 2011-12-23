#lang racket/base
(require racket/class "utils.rkt")
(define dgraph%
  (class object%
    (field [arc-list '()]
           [vtx-list '()])
    
    (define/public (arcs)     arc-list)
    (define/public (vertices) vtx-list)
    
    (define/public (get-outgoing vtx)
      (send vtx get-outgoing))
    
    (define/public (get-incoming vtx)
      (send vtx get-incoming))
    
    (define/public (get-opposite vtx arc)
      (let ([t (send arc get-vertices)])
        (cond 
          [(eq? vtx (car t)) (cdr t)]
          [(eq? vtx (cdr t)) (car t)]
          [else (error 'get-opposite "vertex not attached to arc ~a ~a" vtx arc)]
        )))
    
    (define/public (end-vertices arc)
      (send arc end-vertices))
    
    (define/public (adjacent? vtx1 vtx2)
      (for/or ([e (send vtx1 outgoing)])
        (eq? vtx2 (get-opposite vtx1 e))))
    
    (define/public (add-vertex vtx)
      (push! vtx-list vtx))
    
    (define/public (add-arc v1 v2 arcdata)
      (let ([e (make-object darc% v1 v2 arcdata)])
        (push! arc-list e)))
    
    ))

