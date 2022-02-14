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
                  (set! *game-paused?* #f))
                 (else
                  (void))))
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
           (if (<= (send *player* get-allowed-jumps) 0)
               (void)
               (begin
                 (send *player* set-y-speed! (- (send *player* get-y-speed) jump-height))
                 (send *player* set-allowed-jumps! (- (send *player* get-allowed-jumps) 1)))))
          ((or (eq? key 'left) (eq? key #\a))
           (send *player* set-continous-speed! (- (send *player* get-movement-speed))))
          ((or (eq? key 'right) (eq? key #\d))
           (send *player* set-continous-speed! (send *player* get-movement-speed)))
          ((or (eq? release-key 'left) (eq? release-key #\a)
               (eq? release-key 'right) (eq? release-key #\d))
           (send *player* set-continous-speed! 0))
          (else
           (void)))))

;Begränsar spelarens hastighet
(define (friction)
  (if (< (abs (send *player* get-x-speed)) 0.5)
      (send *player* set-x-speed! 0)
      (send *player* set-x-speed! (- (send *player* get-x-speed) (/ (send *player* get-x-speed) 6))))
  (if (< (abs (send *player* get-y-speed)) 0.01)
      (send *player* set-y-speed! 0)
      (send *player* set-y-speed! (- (send *player* get-y-speed) (/ (send *player* get-y-speed) 6)))))

;Återställer spelaren till rummets början
(define (player-death)
  (let* ((world (send *player* get-world))
         (room (send world get-room))
         (start-x-y (send room get-start-pos)))
    (send *player* set-x-speed! 0)
    (send *player* set-y-speed! 0)
    (send *player* set-continous-speed! 0)
    (send *player* move! (car start-x-y) (cdr start-x-y))))

;Uppdaterar spelarens position varje tick, med hänsyn till kollision osv.
(define (act-player)
  (let* ((x-pos (send *player* get-x-pos))
         (y-pos (send *player* get-y-pos))
         (x-new (+ x-pos (send *player* get-x-speed)))
         (y-new (+ y-pos (send *player* get-y-speed)))
         (room (send *player* get-room)))
    
    ;Applicera gravitation på spelaren
    (define (gravity)
      (send *player* set-y-speed! (+ (send *player* get-y-speed) (send (send *player* get-room) get-gravity)))
      (send *player* move! x-new y-new))
    (if (or (collision? x-pos y-pos) (collision? (+ x-pos 19) y-pos)
            (collision? x-pos (+ y-pos 19)) (collision? (+ x-pos 19) (+ y-pos 19)))
        (send *player* move! (* (round (/ x-pos 20)) 20) (* (round (/ y-pos 20)) 20))
        (void))
    (cond
      ((or (send (tile-type x-new y-new) exit?);entering an exit tile?
           (send (tile-type x-new (+ y-new 19)) exit?)
           (send (tile-type (+ x-new 19) y-new) exit?)
           (send (tile-type (+ x-new 19) (+ y-new 19)) exit?))
       (player-death)
       (send (send *player* get-world) exit-room *player* *menu*))
      ((or (send (tile-type x-new y-new) death?);entering a death tile?
           (send (tile-type (+ x-new 10) (+ y-new 20)) death?) 
           (send (tile-type (+ x-new 19) y-new) death?) 
           (send (tile-type (+ x-new 10) (+ y-new 20)) death?))
       (player-death))
      
      ((or (collision? x-pos (+ y-pos 20)) (collision? (+ x-pos 19) (+ y-pos 20)));down collision?
       (send *player* set-allowed-jumps! 2)
       
       (cond
         ((and (collision? x-pos (+ y-pos 20));down and left collision?
               (collision? (- x-pos 1) (+ y-pos 19)))
          (cond ((and (> x-pos x-new) (< y-pos y-new));left and down
                 (void))
                ((and (> x-pos x-new) (> y-pos y-new));left and up
                 (send *player* move! x-pos y-new))
                ((and (< x-pos x-new) (< y-pos y-new));right and down
                 (send *player* move! x-new y-pos))
                ((and (< x-pos x-new) (> y-pos y-new));right and up
                 (send *player* move! x-new y-new))
                ((and (= x-pos x-new) (> y-pos y-new));standing still and up
                 (send *player* move! x-new y-new))
                (else
                 (void))))
         
         ((and (collision? (+ x-pos 19) (+ y-pos 20));down and right collision?
               (collision? (+ x-pos 20) (+ y-pos 19)))
          (cond ((and (> x-pos x-new) (< y-pos y-new));left and down
                 (send *player* move! x-new y-pos))
                ((and (> x-pos x-new) (> y-pos y-new));left and up
                 (send *player* move! x-new y-new))
                ((and (< x-pos x-new) (< y-pos y-new));right and down
                 (void))
                ((and (< x-pos x-new) (> y-pos y-new));right and up
                 (send *player* move! x-pos y-new))
                ((and (= x-pos x-new) (> y-pos y-new));standing still and up
                 (send *player* move! x-new y-new))
                (else
                 (void))))
         
         ((< y-pos y-new);endast down collision
          (send *player* move! x-new y-pos))
         ((> y-pos y-new)
          (send *player* move! x-new y-new))
         (else
          (send *player* move! x-new y-new))))
      
      ((or (collision? x-pos (- y-pos 1)) (collision? (+ x-pos 19) (- y-pos 1)));up collision?
       (gravity)
       (cond
         ((and (collision? x-pos (- y-pos 1));up and left collision?
               (collision? (- x-pos 1) y-pos))
          (cond ((and (> x-pos x-new) (< y-pos y-new));left and down
                 (send *player* move! x-pos y-new))
                ((and (> x-pos x-new) (> y-pos y-new));left and up
                 (void))
                ((and (< x-pos x-new) (< y-pos y-new));right and down
                 (send *player* move! x-new y-new))
                ((and (< x-pos x-new) (> y-pos y-new));right and up
                 (send *player* move! x-new y-pos))
                ((and (= x-pos x-new) (< y-pos y-new));standing still and down
                 (send *player* move! x-new y-new))
                (else
                 (void))))
         
         ((and (collision? (+ x-pos 19) (- y-pos 1));up and right colission?
               (collision? (+ x-pos 20) y-pos))
          (cond ((and (> x-pos x-new) (< y-pos y-new));left and down
                 (send *player* move! x-new y-new))
                ((and (> x-pos x-new) (> y-pos y-new));left and up
                 (send *player* move! x-new y-pos))
                ((and (< x-pos x-new) (< y-pos y-new));right and down
                 (send *player* move! x-pos y-new))
                ((and (< x-pos x-new) (> y-pos y-new));right and up
                 (void))
                ((and (= x-pos x-new) (< y-pos y-new));standing still and down
                 (send *player* move! x-new y-new))
                (else
                 (void))))
         
         ((> y-pos y-new);only up collision
          (send *player* move! x-new y-pos))
         ((< y-pos y-new)
          (send *player* move! x-new y-new))
         (else
          (send *player* move! x-new y-new))))
      
      ((or (collision? (- x-pos 1) y-pos) (collision? (- x-pos 1) (+ y-pos 19)));left collision?
       (gravity)
       (if (< x-pos x-new)
           (send *player* move! x-new y-new)
           (send *player* move! x-pos y-new)))
      ((or (collision? (+ x-pos 20) y-pos) (collision? (+ x-pos 20) (+ y-pos 19)));right collision?
       (gravity)
       (if (> x-pos x-new)
           (send *player* move! x-new y-new)
           (send *player* move! x-pos y-new)))
      
      (else ;no collision
       (if (>= (send *player* get-allowed-jumps) 2)
           (send *player* set-allowed-jumps! 1)
           (void))
       (gravity)
       (send *player* move! x-new y-new)))))

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
    (cond ((eq? key #\space)
           (print "hello"))
          (else
           (void)))))

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