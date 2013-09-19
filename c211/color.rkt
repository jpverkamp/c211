#lang racket

(require 
 racket/flonum)

(define band? (or/c 'red 'green 'blue 0 1 2))

(provide 
 band?
 (contract-out
  [color        (-> byte? byte? byte? color?)]
  [color-equal? (-> color? color? boolean?)]
  [color-ref    (-> color? band? byte?)]
  [color-set!   (-> color? band? byte? void)]
  [color?       (-> any/c boolean?)]
  [black     color?] [darkgray  color?] [gray      color?] [lightgray color?]
  [white     color?] [red       color?] [green     color?] [blue      color?]
  [yellow    color?] [cyan      color?] [magenta   color?] [orange    color?]
  [pink      color?]))

(define-struct color (r g b) #:transparent #:mutable)

(define (color-equal? color1 color2)
  (equal? color1 color2))

(define (color-ref color band)
  (case band
    [(0 red)   (color-r color)]
    [(1 green) (color-g color)]
    [(2 blue)  (color-b color)]))

(define (color-set! color band value)
  (case band
    [(0 red)   (set-color-r! color value)]
    [(1 green) (set-color-g! color value)]
    [(2 blue)  (set-color-b! color value)]))

(define black     (color 0 0 0))
(define darkgray  (color 84 84 84))
(define gray      (color 192 192 192))
(define lightgray (color 205 205 205))
(define white     (color 255 255 255))
(define red       (color 255 0 0))
(define green     (color 0 255 0))
(define blue      (color 0 0 255))
(define yellow    (color 255 255 0))
(define cyan      (color 0 255 255))
(define magenta   (color 255 0 255))
(define orange    (color 255 127 0))
(define pink      (color 188 143 143))
