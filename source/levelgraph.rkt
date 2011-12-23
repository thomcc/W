#lang racket
(require racket/class 
         racket/set 
         racket/list 
         "utils.rkt"
         "tile.rkt")


;; rooms
(provide (struct-out room) add-path-in add-path-out)
(struct room (paths-in paths-out rdata) #:transparent #:mutable)

(define (add-path-in rm path)
  (struct-copy room rm [paths-in (cons path (room-paths-in rm))]))

(define (add-path-out rm path)
  (struct-copy room rm [paths-out (cons path (room-paths-out rm))]))

(define (adjacent? room1 room2) 
  (for/or ([p (room-paths-out room1)])
    (eq? (get-opposite room1 p)
         room2)))

;; paths
(provide (struct-out path) make-path path-rooms opposite)

(struct path (src dst pdata) #:transparent #:mutable)

(define (make-path src dst pdata)
  (let ((p (path #f #f pdata)))
    (set-room-paths-out! p (add-path-out p))
    (set-room-paths-in!  p (add-path-in p))
    p))

(define (path-rooms path) (cons (path-src path) (path-dst path)))

(define (opposite p rm)
  (let ((rs (path-rooms p)))
    (unless (or (eq? rm (car rs)) (eq? rm (cdr rs)))
      (error 'opposite "room not connected to path p: ~a rm: ~a" p rm))
    (if (eq? rm (car rs)) (car rs) (cdr rs))))



;; levels
(provide (struct-out level)
         make-level add-room
         add-path connect-rooms)

(struct level (rooms paths) #:transparent #:mutable)

(define (make-level [rooms '()] [paths '()]) (level rooms paths))

(define (add-room lvl rm)  
  (struct-copy level lvl [rooms (cons rm  (level-rooms lvl))]))

(define (add-path lvl pth) 
  (struct-copy level lvl [paths (cons pth (level-paths lvl))]))

(define (connect-rooms lvl rm1 rm2 pdata) (add-path lvl (make-path rm1 rm2 pdata)))      
      
      

