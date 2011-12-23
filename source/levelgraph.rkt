#lang racket
(require racket/class 
         racket/set 
         racket/list 
         "utils.rkt"
         "tile.rkt")

(define room%
  (class object% (super-new)
    
    
    ))

(define path%
  (class object% (super-new)
    
    
    ))


(define level%
  (class object% (super-new)
    (define/public (get-rooms) (void)) 
    (define/public (get-paths) (void))
    
    (define/public (get-outgoing room) (void))
    (define/public (get-incoming room) (void))
    
    (define/public (get-end-rooms path) (void))
    
    (define/public (add-room room) (void))
    (define/public (add-path room room type) (void))
    
    (define/public (remove-room room) (void))
    (define/public (remove-path path) (void))
    
    (define/public (connected? room room) (void))
      
    ))

    
      
      
      
      
      

