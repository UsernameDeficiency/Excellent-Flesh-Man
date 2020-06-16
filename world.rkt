#lang racket/gui
(provide world%)

(define world%
  (class object%
    (init-field room-list
                [background 'world%-background-missing]
                [original-room-list room-list])
    (define/public (get-room)
      (car room-list))
    
    (define/public (get-background)
      background)
    
    ;Flytta spelaren till nästa rum alt. menyn om världen är slut
    (define/public (exit-room player menu)
      (if (null? (cdr room-list))
          (begin (send player set-world! menu)
                 (set! room-list original-room-list))
          (set! room-list (cdr room-list))))
    (super-new)))