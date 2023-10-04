#lang racket/gui
(require quickscript)

;;; Author: Stephen De Gabrielle https://github.com/spdegabrielle
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/7

(script-help-string "Eyeballs are following you.")

(define (eye-canvas-mixin %)
  (class %
    (init-field (eye-diameter 100))
    (inherit refresh get-dc client->screen screen->client get-top-level-window)
    (define pupil-diameter (/ eye-diameter 3))
    (define pupil-r (* 1/2 pupil-diameter))
    (define r (/ eye-diameter 2))
    
    (define/override (on-paint)
      ;save the state
      (define dc (get-dc))
      (define pen (send dc get-pen))
      (define brush (send dc get-brush))
      (define f (get-top-level-window))
      ;; now draw the eye
      (send dc set-pen "black" 1 'solid)
      (send dc set-brush "white" 'solid)
      (send dc draw-ellipse 0 0 eye-diameter eye-diameter)
      
      ;As for the magic number, the difference between 
      ;get-current-mouse-state and client->screen may be 
      ;get-display-left-top-inset. 

      (define-values (not-used-x fsy) (get-display-left-top-inset))


      (define-values (ms l) (get-current-mouse-state))
      (define mouse-sx (round (send ms get-x))) ; screen coords
      (define mouse-sy (+ fsy (round (send ms get-y)))) 
      (define-values (mcx mcy) (send this screen->client mouse-sx mouse-sy))
      (define-values (screen-eye-x screen-eye-y) (client->screen r r))
      (define Δx (- screen-eye-x mouse-sx))
      (define Δy (- screen-eye-y mouse-sy))
      (define mag (magnitude (make-rectangular Δx Δy)))

      (if (< mag (- r pupil-r))
          (begin
            (send dc set-brush "black" 'solid)
            (send dc draw-ellipse (- mcx pupil-r) (- mcy pupil-r) pupil-diameter pupil-diameter))
          (let ((direction (atan Δy Δx)))
            (define pupilΔx (- (round (* (cos direction) (* r 2/3)))))
            (define pupilΔy (- (round (* (sin direction) (* r 2/3)))))
            (define (tocentre n) (- (+ r n) pupil-r))
            (define px (tocentre pupilΔx))
            (define py (tocentre pupilΔy))
            (send dc set-brush "black" 'solid)
            (send dc draw-ellipse px py pupil-diameter pupil-diameter)))
      (send dc set-pen pen)
      (send dc set-brush brush)
      (super on-paint))
    (super-new [style '(transparent)])))


(define-script eyes
  #:label "Eyes"
  #:menu-path ("&Games and fun")
  #:help-string "Eyeballs are following you."
  #:persistent
  (λ (selection)

    (define frame (new frame% [label "Eyes"] [width 80] [height 90]))
    (define h (new horizontal-panel% [parent frame]))
    (define c (new (eye-canvas-mixin canvas%) [parent h](eye-diameter 40)))
    (define c2 (new (eye-canvas-mixin canvas%) [parent h](eye-diameter 40)))
    (send frame show #t)

    (define t (new timer% 
                   [notify-callback (λ () (send c refresh)(send c2 refresh))]	 
                   [interval 100]	 
                   [just-once? #f]))
    #f))

(module+ main
  (define frame (new frame% [label "Eyes"] [width 80] [height 90]))
  (define h (new horizontal-panel% [parent frame]))
  (define c (new (eye-canvas-mixin canvas%) [parent h](eye-diameter 40)))
  (define c2 (new (eye-canvas-mixin canvas%) [parent h](eye-diameter 40)))
  (send frame show #t)

  (define t (new timer% 
                 [notify-callback (λ () (send c refresh)(send c2 refresh))]	 
                 [interval 100]	 
                 [just-once? #f])))
