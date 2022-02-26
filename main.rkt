#lang racket/gui
(require "world-init.rkt")

#| Player movement |#
(define *game-paused?* #f);Kanske borde det här vara ett fält i character%

;Läser in tangentbordskommandon från spelaren
(define (interaction-loop key-event)
  (let ((key (send key-event get-key-code))
        (release-key (send key-event get-key-release-code))
        (player-speed (send *player* get-movement-speed))
        (jump-height (send *player* get-jump-height))
        (continous-speed (send *player* get-continous-speed)))
    (cond (*game-paused?*
           (cond ((eq? key 'escape)
                  (send *game-timer* start 16 #f)
                  (send *continous-x-timer* start 16 #f)
                  (set! *game-paused?* #f))
                 ((eq? key #\r)
                  (player-death)
                  (send *game-timer* start 16 #f)
                  (send *continous-x-timer* start 16 #f)
                  (set! *game-paused?* #f))))
          ((eq? key 'escape)
           (set! *game-paused?* #t)
           (send *game-timer* stop)
           (send *continous-x-timer* stop)
           (send *player* set-continous-speed! 0)
           (send (send *graphics-canvas* get-dc) set-font (make-object font% 33 'default))
           (send (send *graphics-canvas* get-dc) draw-text "Paused" 10 10)
           (send (send *graphics-canvas* get-dc) set-font (make-object font% 16 'default))
           (send (send *graphics-canvas* get-dc) draw-text "Press r to DIE!" 10 55))
          ((eq? key #\space)
           (unless (<= (send *player* get-allowed-jumps) 0)
                 (send *player* set-y-speed! (- (send *player* get-y-speed) jump-height))
                 (send *player* set-allowed-jumps! (- (send *player* get-allowed-jumps) 1))))
          ((or (eq? key 'left) (eq? key #\a))
           (send *player* set-continous-speed! (- (send *player* get-movement-speed))))
          ((or (eq? key 'right) (eq? key #\d))
           (send *player* set-continous-speed! (send *player* get-movement-speed)))
          ((or (eq? release-key 'left) (eq? release-key #\a)
               (eq? release-key 'right) (eq? release-key #\d))
           (send *player* set-continous-speed! 0)))))

;Begränsar spelarens hastighet
(define (friction)
  (let ((x-speed (send *player* get-x-speed))
        (y-speed (send *player* get-y-speed)))
    (if (< (abs x-speed) 0.5)
        (send *player* set-x-speed! 0)
        (send *player* set-x-speed! (- x-speed (/ x-speed 6))))
    (if (< (abs y-speed) 0.01)
        (send *player* set-y-speed! 0)
        (send *player* set-y-speed! (- y-speed (/ y-speed 6))))))

;Återställer spelaren till rummets början
(define (player-death)
  (let* ((world (send *player* get-world))
         (room (send world get-room))
         (start-x-y (send room get-start-pos)))
    (sleep 0.2) ; freeze player for a short while
    (send *player* set-x-speed! 0)
    (send *player* set-y-speed! 0)
    (send *player* set-continous-speed! 0)
    (send *player* move! (car start-x-y) (cdr start-x-y))))

; Check for collision and update player state
(define (act-player)
  (let* ([x-pos (send *player* get-x-pos)]
         [y-pos (send *player* get-y-pos)]
         [x-speed (send *player* get-x-speed)]
         [y-speed (send *player* get-y-speed)]
         [x-new (+ x-pos x-speed)]
         [y-new (+ y-pos y-speed)])
    
    ; Applies gravity to player
    (define (gravity)
      (send *player* set-y-speed! (+ (send *player* get-y-speed) (send (send *player* get-room) get-gravity)))
      (send *player* move! x-new y-new))

    ; TODO: Step from player position using respective speed and stop player pos and speed if collision
    ; Check collision in x direction
    (cond
      ; Collision
      [(collision? x-new y-pos) ; TODO: y-pos or y-new?
       (begin
         (send *player* set-x-speed! 0)
         (set! x-new x-pos))]; TODO: Set pos to edge of tile
      ; TODO: No collision, keep x-new?
      [(not (collision? x-new y-pos))
       ;(send *player* move! x-new y-new)])
       (void)])
    
    ; Check collision in y direction
    (cond
      ; Collision
      [(collision? x-pos y-new) ; TODO: x-pos or x-new?
       (send *player* set-y-speed! 0)
       ; Reset jump count if player landed
       (when (> y-speed 0)
           (send *player* set-allowed-jumps! 2))
       ; TODO: Set pos to edge of tile
       (set! y-new y-pos)]
      ; No collision
      [(not (collision? x-pos y-new))
       (gravity)])
    
    ; Move player to new coordinates
    (send *player* move! x-new y-new)
    
    ; TODO: Check if player on death or exit tile
    (cond [(send (tile-type (send *player* get-x-pos) (send *player* get-y-pos)) exit?)
           (player-death)
           (send (send *player* get-world) exit-room *player* *menu*)]
          [(send (tile-type (send *player* get-x-pos) (send *player* get-y-pos)) death?)
           (player-death)])))

#| Graphics |#
(define *game-window* (new frame%
                           [width 800]
                           [height 600]
                           [min-width 800]
                           [min-height 600]
                           [label "Excellent Flesh Man"]
                           [style '(no-resize-border)]))
(send *game-window* show #t)

(define input-canvas%
  (class canvas%
    (init-field keyboard-handler)
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))

(define (render-graphics canvas dc)
  (let* ((world (send *player* get-world)))
    (cond ((eq? world *menu*)
           (send *menu-window* show #t)
           (send *menu-timer* start 16 #f)
           (send *game-window* show #f)
           (send *game-timer* stop))
          (else
           (friction)
           (act-player)
           (send dc draw-bitmap (send world get-background) 0 0)
           (send (send *player* get-room) render dc)
           (send *player* render dc)))))

(define *graphics-canvas* (new input-canvas%
                               [keyboard-handler interaction-loop]
                               [parent *game-window*]
                               [paint-callback render-graphics]))
(send (send *graphics-canvas* get-dc) set-pen (make-object pen% "none" 0 'transparent))


#| Menu |#
(define *menu-window* (new frame%
                           [width 800]
                           [height 600]
                           [min-width 800]
                           [min-height 600]
                           [label "Excellent Flesh Man"]
                           [style '(no-resize-border)]))

(define (menu-canvas-handler key-event)
  (let ((key (send key-event get-key-code))
        (release-key (send key-event get-key-release-code))
        (player-speed (send *player* get-movement-speed))
        (jump-height (send *player* get-jump-height))
        (allowed-jumps (send *player* get-allowed-jumps)))
    (when (eq? key #\space)
           (print "hello"))))

(define (render-menu canvas dc)
  (let* ((world (send *player* get-world)))
    (cond ((eq? world *menu*)
           (send *menu* render dc))
          (else
           (send *menu-window* show #f)
           (send *game-window* show #t)
           (send *menu-timer* stop)
           (send *game-timer* start 16 #f)))))

(define *menu-canvas* (new input-canvas%
                           [keyboard-handler menu-canvas-handler]
                           [parent *menu-window*]
                           [paint-callback render-menu]))
(send (send *menu-canvas* get-dc) set-pen (make-object pen% "none" 0 'transparent))

(define (world1-proc button event)
  (send *player* set-world! *world1*))
(define *button-world1* (new button%
                             [parent *menu-window*]
                             [label "World 1"]
                             [callback world1-proc]))

(define (world2-proc button event)
  (let* ((first-room (send *world2* get-room))
         (start-pos (send first-room get-start-pos)))
    (send *player* move! (car start-pos) (cdr start-pos))
    (send *player* set-world! *world2*)))
(define *button-world2* (new button%
                             [parent *menu-window*]
                             [label "World 2"]
                             [callback world2-proc]))

(define (menu-timer-tick)
  (send *menu-canvas* refresh))
(define *menu-timer* (new timer%
                          [notify-callback menu-timer-tick]))
(send *menu-timer* start 16 #f)

#| Timers |#
(define (game-timer)
  (send *graphics-canvas* refresh))
(define *game-timer* (new timer%
                          [notify-callback game-timer]))

;Ser till att rörelsen i x-led är jämn när t.ex. a eller d hålls ner
(define (continous-movement)
  (send *player* move-continously!))
(define *continous-x-timer* (new timer%
                                 [notify-callback continous-movement]))

(send *game-timer* start 16 #f)
(send *continous-x-timer* start 16 #f)