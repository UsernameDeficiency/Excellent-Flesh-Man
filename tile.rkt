#lang racket
(provide tile%)

(define tile%
  (class object%
    (init-field name
                bitmap
                [collision #t]
                [death #f]
                [exit #f])
    (define/public (get-name)
      name)

    (define/public (get-bitmap)
     bitmap)

    (define/public (collision?)
      collision)
    
    (define/public (death?)
      death)

    (define/public (exit?)
      exit)
    (super-new)))