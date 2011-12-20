#lang racket
(require racket/class "utils.rkt" "tile.rkt")
(provide (all-defined-out))
(struct level (data spawn exits) #:transparent #:mutable)


(define (make-level sh spawn exits)
  (let* ([teles '()]
         [screens '()]
         [v (for/vector ([row sh] [y (in-naturals)])
              (for/vector ([p row][x (in-naturals)])
                (cond [(cons? p)
                       (case (car p)
                         [(t) ; teleport contains the position of its destination.
                          (let ((tt (init-teleport (cons x y))))
                            (push! teles (cons tt (cdr p)))
                            tt)]
                         [(1 0) ; screen contains the position of a thing it activates
                          (let ((ss (init-screen (eq? (car p) 1) (cons x y))))
                            (push! screens (cons ss (cdr p)))
                            ss)]
                         [(> v) ; door contains open and locked
                          (init-door (if (eq? (car p) '>) 'lr 'ud) (cons x y) (second p) (third p))])]
                      [else
                       (case p
                         [(g) (init-grass (cons x y))]
                         [(f) (init-flor (cons x y))]
                         [(d) (init-dirt (cons x y))]
                         [(l) (init-lava (cons x y))]
                         [(w) (init-wall (cons x y))]
                         [(s) (init-sand (cons x y))])])))])
    (for-each (λ (t) (set-teleport-dest! (car t) (vref v (cddr t) (cadr t)))) teles)
    (for-each (λ (s) (set-screen-controls! (car s) (if (procedure? (cdr s)) (cdr s)
                                                       (vref v (cddr s) (cadr s))))) screens)
    (level v spawn exits)))

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
          (vector 'w 'w 0 'w 'w 'w 'w '(v #t #t) 'w 'w)
          (vector 'w 'l 'f 'l 'f 'l 'l 'f 'l 'w)
          (vector 'w 'l 'f 'l 'l 'l 'f 'f 'l 'w)
          (vector 'w 'l 'f 'f 'f 'f 'f 'l 'l 'w)
          (vector 'w '(t 4 . 1) 'f 'l 'l 'l 'l  'l 'f '(> #f #f))
          (vector 'w 'w 'w 'w 'w 'w 'w 'w 'w 'w))))
    (vset! ll 0 2 (cons 0 ff))
    ll))
(define (make-exportable level)
      (for/vector ((j (level-data level)))
        (for/vector ((i j))
          (let ((sn (struct-name i)))
            (case sn
              [(screen) (if (screen-on? i) 'screen-on 'screen-off)]
              [(door)   (let ((d (door-dir i))
                              (o (if (door-open? i) "open" "close")))
                          (string->symbol (string-append "door-" (symbol->string d) "-" o)))]
              [else sn])))))

(define (leveldata-map f data)
  (for/vector ([y data])
    (for/vector ([x y])
      (f x))))

(define (level-map f l) (struct-copy level l [data (leveldata-map f (level-data l))]))

(define (leveldata-for-each f d)
  (for ([row d][y (in-naturals)] #:when #t [p row][x (in-naturals)])
    (f d x y)))

(define (level-count f l) (leveldata-count f (level-data l)))

(define (leveldata-count f d)
  (for*/fold ([i 0])
    ([row d]
     [p row]
     #:when (f p))
    (add1 i)))

(define (screens-on+off level)
  (for*/fold ([on 0] [off 0]) 
    ([row level][p row] #:when (screen? p))
    (if (screen-on? p) 
        (values (add1 on) off) 
        (values on (add1 off)))))

(define (on+off-sc-posns level)
  (for*/fold ([ons '()] [offs '()]) ([row level] [p row] #:when (screen? p))
    (if (screen-on? p) 
        (values (cons (cons (tile-x p) (tile-y p)) ons) offs)
        (values ons (cons (cons (tile-x p) (tile-y p)) offs)))))

(define (screens-on level)  (let-values ([(on off) (screens-on+off level)]) on))
(define (screens-off level) (let-values ([(on off) (screens-on+off level)]) off))

(define (lightf x y)
  (λ (level _ [init? #f])
    (let ((nbors (filter screen? (cond [(or (= x 0) (= x 9)) (list (vref level (+ y 1) x) (vref level (- y 1) x))]
                                       [(or (= y 0) (= y 5)) (list (vref level  y (+ x 1)) (vref level y (- x 1)))]
                                       [else '()]))))
      (for-each (λ (s) (set-screen-on?! s (not (screen-on? s)))) nbors))
    (let-values ([(on off) (screens-on+off level)]
                 [(ons offs) (on+off-sc-posns level)])
      (unless init?
        (cond [(= 0 on); y'done lost.
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
  (let ([l (shuffle (for*/list ([row (level-data lo)] [p row] #:when (screen? p)) p))])
    (for ([i (in-range 10)]) 
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

(define (nxlevel)
      #(#(w w w w w w w w w w)
        #(w d d d f f g g g w)
#((> #f #t) d d d f f g g g w)
        #(w d d d f f g g g w)
        #(w d d d f f g g g w)
        #(w w w w w w w w w w)))




(define-values (level:lava level:start level:nx level:lights-out)
  (letrec ((lv (λ () (make-level (lavalevel)  (cons 7 0) `(((7 . -1) . ,st) ((10 . 4) . ,lo)))))
           (st (λ () (make-level (startlevel) (cons 7 3) `(((7 . 5) . ,lv)))))
           (nx (λ () (make-level (nxlevel) (cons 1 2) '())))
           (lo (λ () (scramble (make-level (lightsout)  (cons 1 1) `(((10 . 2) . ,nx)))))))
    (values lv st nx lo)))



