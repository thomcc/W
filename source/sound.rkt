#lang racket/base
(require racket/gui/base racket/runtime-path "utils.rkt")
(define-runtime-path bump "../res/bump.wav")
(define-runtime-path door "../res/door.wav")
(define-runtime-path game-over  "../res/gameover.wav")
(define-runtime-path game-start "../res/gamestart.wav")
(define-runtime-path screen "../res/screen.wav")
(define-runtime-path zap "../res/zap.wav")
(define-runtime-path solve "../res/solve.wav")
(define-runtime-path locked "../res/locked.wav")
(define (get-sound s)
  (case s
    [(zap)                            zap]
    [(bump)                           bump]
    [(game-start start-game new-game) game-start]
    [(game-over)                      game-over]
    [(screen)                         screen]
    [(door)                           door]
    [(locked)                         locked]
    [(solve win new-level)            solve]
    [else (printf "couldn't find sound: ~a~n" s) #f]))
(provide play-effect play-effects)
(define (play-effects elist)
  (for-each play-effect elist))
(define (play-effect effectname [async? #t])
  (let ((fxp (get-sound effectname)));(alist-get effectname sounds-alist (λ _ (printf "couldnt find sound: ~a~n" effectname) #f))))
    (when fxp
      (play-sound fxp async?))))