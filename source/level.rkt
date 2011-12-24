#lang racket/base
(require racket/list racket/class "utils.rkt" "tile.rkt")
(provide (all-defined-out))
(struct level (data spawn exits) #:transparent #:mutable)


(define (make-level sh spawn exits)
  (let* ([teles '()]
         [screens '()]
         [v (for/vector ([row (in-vector sh)] [y (in-naturals)])
              (for/vector ([p (in-vector row)][x (in-naturals)])
                (cond [(pair? p)
                       (case (car p)
                         [(t) ; teleport contains the position of its destination.
                          (let ((tt (init-teleport (cons x y))))
                            (push! teles (cons tt (cdr p)))
                            tt)]
                         [(1 0) ; screen contains the position of a thing 
                               ; it activates or a function
                          (let ((ss (init-screen (eq? (car p) 1) (cons x y))))
                            (push! screens (cons ss (cdr p)))
                            ss)]
                         [(> v) ; door contains open and locked
                          (init-door (if (eq? (car p) '>) 'lr 'ud) 
                                     (cons x y) (second p) (third p))])]
                      [else
                       (case p
                         [(g) (init-grass (cons x y))]
                         [(f) (init-flor (cons x y))]
                         [(d) (init-dirt (cons x y))]
                         [(l) (init-lava (cons x y))]
                         [(w) (init-wall (cons x y))]
                         [(y) (init-yes-f (cons x y))]
                         [(n) (init-no-f  (cons x y))]
                         [(s) (init-sand (cons x y))])])))])
    (for ([t (in-list teles)])
      (set-teleport-dest! 
       (car t)
       (vref v (cddr t) (cadr t))))
    
    (for ([s (in-list screens)])
      (set-screen-controls! 
       (car s)
       (if (procedure? (cdr s))
           (cdr s)
           (vref v (cddr s) (cadr s)))))
    
    (level v spawn exits)))

  
                                      


;; this is, i think, what i would like to write.
#;(define-room (start #:dimensions '(10 . 6))
  
    (fill    #:style 'floor)
    (outline #:style 'wall)
             
    (screen     #:at '(1 . 0) #:toggles '(7 . 5)  #:states 2)
    (teleporter #:at '(2 . 2) #:zaps-to '(5 . 4))
    (teleporter #:at '(5 . 4) #:zaps-to '(2 . 2))
    
    
    (door #:at '(7 . 5) #:faces 'up/down #:locked #t) 
    (door #:at '(4 . 3) #:faces 'left/right)
    
    
    (draw-line  #:from '(4 . 0) #:to '(4 . 5) #:style 'wall)
    (draw-line  #:from '(6 . 5) #:to '(4 . 5) #:style 'wall 
                #:except-over '(door))
    
    )
;; now, do i got the chops ;)

(struct room (data width height) #:transparent #:mutable)

(define (room-contains? room pt)
  (and (non-negative-integer? (car pt))
       (non-negative-integer? (cdr pt))
       (< (car pt) (room-width  room))
       (< (cdr pt) (room-height room))))

(define (get-tile [room (current-room)] #:at posn)
  (if (room-contains? room posn)
      (vref room (cdr posn) (car posn))
      #f))

(define (set-tile [room (current-room)] #:at posn #:style tile)
  (if (room-contains? room posn) 
      (vset! room (cdr posn) (car posn) tile)
      (printf "warning! position out of bounds: position: ~a room: ~a style: ~a"
              posn room tile)))

(define (draw-line [room (current-room)] 
                   #:from        from-posn 
                   #:to          to-posn 
                   #:style       [style 'wall]
                   #:except-over [preserve '()])
  (let* ([dx (- (car to-posn) (car from-posn))]
         [dy (- (cdr to-posn) (cdr from-posn))]
         [steps (if (> (abs dx) (abs dy)) (abs dx) (abs dy))]
         [xi (/ dx steps)]
         [yi (/ dy steps)])
    (set-tile room #:at from-posn #:style tile)
    (let ((x (car from-posn)) (y (cdr from-position)))
      (for ([k (in-range steps)])
        (set! x (+ xi x))
        (set! y (+ yi y))
        (set-pixel room #:at `(,(floor* x) . ,(floor* y)) #:style tile)))))




(define (startlevel)
  #(#(w (0 7 . 5) w  w        w w        w  w w w)
    #(w f  f        f w  f        f  f f w)
    #(w f (t 5 . 4) f w  f        w  f f w)
    #(w f  f        f w  f (> #f #f) f f w)
    #(w f  f        f w (t 2 . 2) w  f f w)
    #(w w  w        w w  w        w  (v #f #t) w w)))


(define (ff level . _)
  (let ([tele (vref level 4 1)]
        [dst0 (vref level 1 4)]
        [dst1 (vref level 4 8)])
    (if (eq? (teleport-dest tele) dst0)
        (set-teleport-dest! tele dst1)
        (set-teleport-dest! tele dst0))))


(define (lavalevel)
  (let ((ll
         (vector 
          (vector 'w 'w 0 'w 'w 'w 'w '(v #f #t) 'w 'w)
          (vector 'w 'l 'f 'l 'f 'l 'l 'f 'l 'w)
          (vector 'w 'l 'f 'l 'l 'l 'f 'f 'l 'w)
          (vector 'w 'l 'f 'f 'f 'f 'f 'w 'l 'w)
          (vector 'w '(t 4 . 1) 'f 'l 'l 'l 'l  'l 'f '(> #f #f))
          (vector 'w 'w 'w 'w 'w 'w 'w 'w 'w 'w))))
    (vset! ll 0 2 (cons 0 ff))
    ll))
(define (make-exportable level)
      (for/vector ((j (in-vector (level-data level))))
        (for/vector ((i (in-vector j)))
          (let ((sn (struct-name i)))
            (case sn
              [(screen) (if (screen-on? i) 'screen-on 'screen-off)]
              [(door)   (let ((d (door-dir i))
                              (o (if (door-open? i) "open" "close")))
                          (string->symbol (string-append "door-" (symbol->string d) "-" o)))]
              [else sn])))))

(define (leveldata-map f data)
  (for/vector ([y (in-vector data)])
    (for/vector ([x (in-vector y)])
      (f x))))

(define (level-map f l) (struct-copy level l [data (leveldata-map f (level-data l))]))

(define (leveldata-for-each f d)
  (for ([row (in-vector d)][y (in-naturals)] #:when #t 
                           [p (in-vector row)][x (in-naturals)])
    (f d x y)))

(define (level-count f l) (leveldata-count f (level-data l)))

(define (leveldata-count f d)
  (for*/fold ([i 0])
    ([row (in-vector d)]
     [p (in-vector row)]
     #:when (f p))
    (add1 i)))

(define (screens-on+off level)
  (for*/fold ([on 0] [off 0]) 
    ([row (in-vector level)][p (in-vector row)] #:when (screen? p))
    (if (screen-on? p) 
        (values (add1 on) off) 
        (values on (add1 off)))))

(define (on+off-sc-posns level)
  (for*/fold ([ons '()] [offs '()]) 
    ([row (in-vector level)] [p (in-vector row)] #:when (screen? p))
    (if (screen-on? p) 
        (values (cons (cons (tile-x p) (tile-y p)) ons) offs)
        (values ons (cons (cons (tile-x p) (tile-y p)) offs)))))

(define (screens-on level)  (let-values ([(on off) (screens-on+off level)]) on))
(define (screens-off level) (let-values ([(on off) (screens-on+off level)]) off))

(define (lightf x y)
  (λ (level _ [init? #f] . rst)
    (let ((nbors (filter screen? (cond [(or (= x 0) (= x 9)) (list (vref level (+ y 1) x) (vref level (- y 1) x))]
                                       [(or (= y 0) (= y 5)) (list (vref level  y (+ x 1)) (vref level y (- x 1)))]
                                       [else '()]))))
      (for-each (λ (s) (set-screen-on?! s (not (screen-on? s)))) nbors))
    (let-values ([(on off) (screens-on+off level)]
                 [(ons offs) (on+off-sc-posns level)])
      (unless init?
        (cond [(= 0 on); y'lost.
               (leveldata-for-each
                (λ (ld x y) 
                  (when (random-boolean) 
                  (vset! ld y x (init-lava)))) level)]
              [(= 0 off) (vset! level 2 9 (init-flor))]
            )))))

(define (lightsout)
  (build-vector 
   6 
   (λ (y)
     (build-vector 
      10 
      (λ (x)
        (cond [(not (or (= x 0) (= y 0) (= x 9) (= y 5))) 'f]
              [(or (and (= x 0) (= y 0))
                   (and (= x 0) (= y 5))
                   (and (= x 9) (= y 0))
                   (and (= x 9) (= y 5))) 'w]
              [else
               (cons 1 (lightf x y))]))))))

                                       
(define (scramble lo)
  (let ([l (shuffle 
            (for*/list 
                ([row (in-vector (level-data lo))] 
                 [p (in-vector row)] #:when (screen? p)) 
              p))])
    (for ([i (in-range 15)]) 
      (let ((s (random-element l)))
        (use-screen s)
        ((screen-controls s) (level-data lo) #t #t))))
  lo)

(define (emptylevel)
  #(#(w w w w w w w w w w)
    #(w f f f f f f f f w)
    #(w f f f f f f f f w)
    #(w f f f f f f f f w)
    #(w f f f f f f f f w)
    #(w w w w w w w w w w)))

(define (mzlevel)
      #(#(w w w w(v #f #t) w w w w w)
        #(w d d d f        f g g g w)
#((> #f #t) d d d f        f g g g w)
        #(w d d d f        f g g g(> #f #t))
        #(w d d d f        f g g g w)
        #(w w w w(v #t #f) w w w w w)))
(define (code-level)
       #(#(w w w w(v #f #t) w w w w w)
         #(f f f w f        f w f f f)
         #(w w w w f        f w w w w)
         #(w f f f f        f f f f w)
         #(w y n n y        n y y n w)
         #(w 0 0 0 0        0 0 0 0 w)  ; replace this one
         ))
(define (INFINITE-GREEN-PASTURES)
  #(#(d d d g g d d g g g)
    #(g d g g g d g d g g)
    #(g d g g g d d g g g)
    #(g g d d d g g d d d)
    #(g g d g d g g d g d)
    #(g g d d d g g d d d)))
(define (mk-igp)
  (make-level (INFINITE-GREEN-PASTURES) (cons 1 1) 'wrap-around))
  



(define (funcvec)
  (let ()
    (define solution '(0 1 1 0 1 0 0 1))
    (define failure  '(1 0 0 1 0 1 1 0))
    (define (get-screens ld) (build-list 8 (λ (p) (vref ld 5 (add1 p)))))
    (define (make-state=? s) (λ (ld) (equal? s (map (λ (s) (if (screen-on? s) 1 0)) (get-screens ld)))))
    (define solved? (make-state=? solution))
    (define failed? (make-state=? failure))
    (define (permute-wrong ld) 
      (let ([w (map = solution (map (λ (s) (if (screen-on? s) 1 0)) (get-screens ld)))])
        (for ([p (in-list w)] [x (in-range 1 9)])
          (vset! ld 4 x (if p (init-yes-f)(init-no-f))))))
    (define (check-ld ld game) 
      (cond [(solved? ld) (send game teleport-player (vref ld 1 7))]
            [(failed? ld) (send game teleport-player (vref ld 1 2))]
            )(permute-wrong ld))
    (define (i-screen x)
      (let ((s (init-screen #f (cons x 5))))
        (set-screen-controls! s (λ (ld game . _) (check-ld ld game)))
        s))
    (build-vector 10 (λ (x) (if (or (= x 0) (= x 9)) (init-wall) (i-screen x))))))
; todo: better datastructure for representing levels
; directed graph?



(define-values (level:lava level:start level:lights-out level:maze level:code)
  (letrec ((lv (λ () (make-level (lavalevel)  (cons 7 1) `(((7 . -1) . ,st) ((10 . 4) . ,lo)))))
           (st (λ () (make-level (startlevel) (cons 7 3) `(((7 . 5) . ,lv)))))
           (mz (λ () (make-level (mzlevel) (cons 1 2)    `(((4 . 6) . ,cl)))))
           (cl (λ () (let ((l (make-level (code-level) 
                                          (cons 4 1)  
                                          `(((-1 . 1) . ,mk-igp) ((10 . 1) . ,mk-igp)))))
                       (vector-set! (level-data l) 5 (funcvec)) l) ))
           (lo (λ () (scramble (make-level (lightsout)  (cons 1 1) `(((10 . 2) . ,mz)))))))
    (values lv st lo mz cl)))



