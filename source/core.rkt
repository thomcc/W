#lang racket/base
(require racket/gui/base)
(provide (all-defined-out))

(define *width*  800)
(define *height* 480)

(define TILES_WIDE 10)
(define TILES_HIGH 6)
(define TILE_SIZE 16)
(define PIX_WIDE (* TILES_WIDE TILE_SIZE))
(define PIX_HIGH (* TILES_HIGH TILE_SIZE))
(define ASPECT_RATIO (/ PIX_WIDE PIX_HIGH))
(define *scale*  (/ *width* PIX_WIDE))

(define *magenta-is-transparent* (make-parameter #t))
(define *debug* (make-parameter #f))
(define *game-name* (make-parameter ""))
