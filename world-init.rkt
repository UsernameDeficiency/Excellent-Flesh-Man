#lang racket/gui
(require "menu.rkt")
(require "tile.rkt")
(require "room.rkt")
(require "world.rkt")
(require "character.rkt")
(require "room-matrices.rkt")
(provide (all-defined-out))

#| Tiles |#
;Skapar en helfärgad 20x20-bitmap
(define (create-platform-image color)
  (let* ((tile-bitmap (make-bitmap 20 20))
         (dc (new bitmap-dc% [bitmap tile-bitmap])))
    (send dc set-brush (make-object brush% color 'solid))
    (send dc set-pen (make-object pen% color 0 'transparent))
    (send dc draw-rectangle 0 0 20 20)
    tile-bitmap))

(define *red-bitmap* (create-platform-image "red"))
(define *green-bitmap* (create-platform-image "green"))
(define *blue-bitmap* (create-platform-image (make-object color% 100 150 240)))
(define *transparent-bitmap* (make-bitmap 20 20))

(define *sky*
  (new tile%
       [name 'sky]
       [bitmap *transparent-bitmap*]
       [collision #f]
       [death #f]))

(define *grass*
  (new tile%
       [name 'grass]
       [bitmap *green-bitmap*]
       [collision #t]
       [death #f]))

(define *fire*
  (new tile%
       [name 'fire]
       [bitmap *red-bitmap*]
       [collision #f]
       [death #t]))

(define brickmap (make-bitmap 20 20))
(send brickmap load-file (open-input-file "graphics/bricks2.gif"))
(define *brick*
  (new tile%
       [name 'brick]
       [bitmap brickmap]
       [collision #t]
       [death #f]))

(define exit-door (make-bitmap 20 20))
(send exit-door load-file (open-input-file "graphics/exit.gif"))
(define *exit*
  (new tile%
       [name 'exit]
       [bitmap exit-door]
       [collision #f]
       [death #t]
       [exit #t]))

;Kopplar ihop siffrorna i room-matrices.rkt med tile-objekt
(define *tile-table* 
  (hash 0 *sky* 1 *grass* 2 *brick* 4 *fire* 'x *exit*))

;Returnerar den tile på plats x, y i spelarens nuvarande room
(define (tile-type x y)
  (hash-ref *tile-table* (send (send *player* get-room) 
                               get-tile (inexact->exact (floor (/ x 20))) (floor (inexact->exact (/ y 20))))))

;Ser om den tile på plats x, y har kollision
(define (collision? x y)
  (send (tile-type x y) collision?))


#| Rooms - *room[worldnr]-[roomnr]* |#
(define *room1-1*
  (new room%
       [tile-matrix *room1-1-matrix*]
       [start-pos (cons 30 450)]))
(send *room1-1* create-bitmap *tile-table*)

(define *room1-2*
  (new room%
       [tile-matrix *room1-2-matrix*]
       [start-pos (cons 20 450)]))
(send *room1-2* create-bitmap *tile-table*)

(define *room2-1*
  (new room%
       [tile-matrix *room2-1-matrix*]
       [start-pos (cons 10 350)]))
(send *room2-1* create-bitmap *tile-table*)

(define *room2-2*
  (new room%
       [tile-matrix *room2-2-matrix*]
       [start-pos (cons 10 170)]))
(send *room2-2* create-bitmap *tile-table*)


#| Worlds |#
(define *world1-background* (make-bitmap 800 600))
(send *world1-background* load-file (open-input-file "graphics/world1.gif"))

(define *world1*
  (new world%
       [room-list (list *room1-1* *room1-2*)]
       [background *world1-background*]))

(define *world2*
  (new world%
       [room-list (list *room2-1* *room2-2*)]
       [background *world1-background*]));Gör en unik background


#| Menu |#
(define *menu*
  (new menu%
       [room *room1-1*]
       [background *world1-background*]))


#| Characters |#
(define *player-bitmap* (make-bitmap 20 20))
(send *player-bitmap* load-file (open-input-file "graphics/player.gif") 'gif/alpha)


(define *player*
  (new character%
       [bitmap *player-bitmap*]
       [world *menu*]))
;Flytta spelaren till första rummets start-pos
(send *player* move! (car (send *room1-1* get-start-pos)) (cdr (send *room1-1* get-start-pos)));Kör när banan laddas efter menyn