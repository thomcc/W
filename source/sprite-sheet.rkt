#lang racket/base
(require racket/gui/base
         racket/class
         racket/dict
         "utils.rkt")
(define (cut-bitmap bmp x y w h)
  (let* ((c (make-bitmap w h))
         (dc (make-object bitmap-dc% c)))
    (send dc draw-bitmap-section bmp 0 0 x y w h)
    c))
(provide sprite-sheet%)
(define sprite-sheet%
  (class object% (super-new)
    (init path 
          [type   'png/alpha] 
          [size   '(16 16)] ; size of sprites
          [offset '(0 0)] ; pixel offsets from top left
          [name-dict #f]
          ; a dict relating position '(x y) to name.
          ; if false, positions will be used directly for the name
          ; if an entry is not specified in the dict, it will not be present
          )
    (define-values (frame-dict dimensions)
      (let* ((bitmap (read-bitmap path type))
             (bw (send bitmap get-width))
             (bh (send bitmap get-height))
             (frames
              (for/hash ([i (in-range (car offset) 
                                      (send bitmap get-width) 
                                      (car size))]
                         [x (in-naturals 0)]
                         #:when #t
                         [j (in-range (cadr offset)
                                      (send bitmap get-height) 
                                      (cadr size))]
                         [y (in-naturals 0)]
                         #:when (or (not name-dict) 
                                    (dict-ref name-dict 
                                              (list x y) 
                                              (λ _ #f))))
                (let ((name (if (not name-dict) 
                                (list x y) 
                                (dict-ref name-dict (list x y)))))
                  (values name (cut-bitmap bitmap i j (car size) (cadr size))))))
             (dimensions (map floor* (list bw bh) size)))
        (values frames dimensions)))
    
    (define/public (get-frame-dict) frame-dict)
    (define/public (get-frame frame-name)
      (dict-ref frame-dict frame-name (λ _ #f)))
    (define/public (get-frames frames)
      (for/list ([name (in-list frames)]) (get-frame name)))))




