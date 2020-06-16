#lang racket
(provide menu%)

(define menu%
  (class object%
    (init-field room
                [background 'menu%-no-background])
    
    (define/public (get-room)
      room)
    
    (define/public (render dc)
      (send dc draw-bitmap background 0 0))
    (super-new)))