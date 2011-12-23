#lang racket
(require racket/class 
         racket/set 
         racket/list 
         "utils.rkt"
         "tile.rkt")

(define room%
  (class object% (super-new)
    (field [paths-out '()]
           [paths-in  '()]
           [room-data #f])
    (define/public (add-outgoing path) (set! paths-out (cons path paths-out)))
    (define/public (add-incoming path) (set! paths-in (cons path paths-in)))
    (define/public (get-outgoing) paths-out)
    (define/public (get-incoming) paths-in)
    ))

(define path%
  (class object% (super-new)
    (field [source #f]
           [dest   #f])
    
    
    ))


(define level%
  (class object% (super-new)
    (field [rooms '()]
           [paths '()])
    
    (define/public (get-outgoing room) (void))
    (define/public (get-incoming room) (void))
    
    (define/public (get-end-rooms path) (void))
    
    (define/public (add-room room) (void))
    (define/public (add-path room room type) (void))
    
    (define/public (remove-room room) (void))
    (define/public (remove-path path) (void))
    
    (define/public (connected? room room) (void))
      
    ))

    
      
      
      
      
      

