#lang racket
(provide character%)

(define character%
  (class object%
    (init-field [bitmap 'character%-no-bitmap]
                [world 'character%-no-world]
                [x-pos 0]
                [y-pos 0]
                [x-speed 0]
                [y-speed 0]
                [movement-speed 1]
                [jump-height 20]
                [allowed-jumps 8]
                [continous-speed 0])
    (define/public (get-world)
      world)
    
    (define/public (set-world! new-world)
      (set! world new-world))
    
    (define/public (get-x-pos)
      x-pos)
    
    (define/public (get-y-pos)
      y-pos)  
    
    (define/public (get-x-speed)
      x-speed)
    
    (define/public (get-y-speed)
      y-speed)
    
    (define/public (set-x-speed! new-speed)
      (set! x-speed new-speed))
    
    (define/public (set-y-speed! new-speed)
      (set! y-speed new-speed))
    
    (define/public (move! x y)
      (set! x-pos x)
      (set! y-pos y))
    
    (define/public (get-room)
      (send world get-room))
    
    (define/public (get-movement-speed)
      movement-speed)
    
    (define/public (get-jump-height)
      jump-height)
    
    (define/public (get-allowed-jumps)
      allowed-jumps)
    
    (define/public (set-allowed-jumps! val)
      (set! allowed-jumps val))
    
    (define/public (get-continous-speed)
      continous-speed)
    
    (define/public (set-continous-speed! val)
      (set! continous-speed val))
    
    ;Flytta karaktären varje tick när t.ex. a eller d hålls ner
    (define/public (move-continously!)
      (set-x-speed! (+ x-speed continous-speed)))
    
    ;Rita upp karaktären på position x-pos, y-pos i dc
    (define/public (render dc)
      (send dc draw-bitmap bitmap x-pos y-pos))
    (super-new)))