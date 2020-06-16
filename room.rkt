#lang racket/gui
(provide room%)

(define room%
  (class object%
    (init-field tile-matrix
                start-pos
                [tile-bitmap 'tile-bitmap-missing]
                [gravity 1])
    (define/public (get-start-pos)
      start-pos)
    
    (define/public (get-gravity)
      gravity)
    
    ;Returnerar siffran som associeras med en tile% i world-init
    (define/public (get-tile column-num row-num)
      (cond ((or (>= row-num 30) (<= row-num -20) (<= column-num -20) (>= column-num 50))
             4)
            ((or (< row-num 0) (< column-num 0) (>= column-num 40))
             1)
            (else
             (list-ref (list-ref tile-matrix row-num) column-num))))
    
    ;Sparar rummets grafikbitmap
    (define/public (create-bitmap tile-table)
      (let* ((new-bitmap (make-bitmap 800 600))
             (dc (new bitmap-dc% [bitmap new-bitmap])))
        (define (render-row tile-row row-num column-num)
          (if (null? tile-row)
              (void)
              (begin
                (send dc draw-bitmap (send (hash-ref tile-table (car tile-row)) get-bitmap)
                      (+ 0 (* column-num 20)) (+ 0 (* row-num 1 20)))
                (render-row (cdr tile-row) row-num (+ 1 column-num)))))
        (define (render-all-rows room-matrix row-num)
          (if (null? room-matrix)
              (set! tile-bitmap new-bitmap)
              (begin
                (render-row (car room-matrix) row-num 0)
                (render-all-rows (cdr room-matrix) (+ 1 row-num)))))
        (render-all-rows tile-matrix 0)))
    
    ;Ritar upp rummets bitmap (kör create-bitmap först)
    (define/public (render dc)
      (send dc draw-bitmap tile-bitmap 0 0))
    (super-new)))