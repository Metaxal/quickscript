#lang racket/base
(require racket/gui/base
         racket/class
         setup/dirs
         "base.rkt"
         (prefix-in lib: "library.rkt"))

(provide make-library-gui)

(define check-sym #\☑)
(define uncheck-sym #\☐)

(define (un/checked-file->check+file cf)
  (define checked? (char=? check-sym (string-ref cf 0)))
  (values checked? (substring cf 2)))

(define (check+file->un/checked-file c f)
  (string-append
   (string (if c uncheck-sym check-sym)
           #\space)
   f))

(define (make-library-gui [the-lib-file library-file])
  (define the-lib (lib:load the-lib-file))

  (define (files-lb-selection-values)
    (define cf (send files-lb get-string-selection))
    (if cf
        (un/checked-file->check+file cf)
        (values #f #f)))


  (define (set-files-lb dir-str)
    (define files
      (map path->string
           (filter (λ(f)(script-file? (build-path dir-str f)))
                   (directory-list dir-str #:build? #f))))
    (define excluded-files (lib:exclusions the-lib dir-str))
    (send files-lb set
          (map (λ(f)(check+file->un/checked-file (member f excluded-files) f))
               files)))

  (define (ex/include-selected-file)
    (define dir (send dir-lb get-string-selection))
    (when dir
      (define-values (checked? file)
        (files-lb-selection-values))
      (when file
        (if checked?
            (lib:exclude! the-lib dir file)
            (lib:include! the-lib dir file))
        (set-files-lb dir))))

  (define (add-directory)
    (define dir
      (get-directory "Choose a script directory to add to the library"
                     fr
                     (find-user-pkgs-dir)))
    (lib:add-directory! the-lib dir)
    (reload-dir-lb)
    (send dir-lb set-string-selection (path->string dir))
    (set-files-lb dir))

  (define (remove-directory dir)
    (lib:remove-directory! the-lib dir)
    (reload-dir-lb))

  (define (reload-dir-lb)
    (send dir-lb clear)
    (send dir-lb set (lib:directories the-lib)))

  (define fr (new frame% [label "Script library"]
                  [min-width 500]
                  [min-height 400]))
  (define panels (new horizontal-panel% [parent fr]))

  (define dir-panel (new vertical-panel% [parent panels]))
  (define dir-lb
    (new list-box% [parent dir-panel]
         [label "Directories"]
         [choices (lib:directories the-lib)]
         [style '(single vertical-label)]
         [callback (λ(lb ev)
                     (define dir (send lb get-string-selection))
                     (when dir
                       (set-files-lb dir)
                       (send bt-dir-remove enable
                             (not (string=? dir
                                            (path-string->string
                                             user-script-dir))))))]))
  (define bt-dir-panel (new horizontal-panel% [parent dir-panel]
                            [stretchable-height #f]
                            [alignment '(center center)]))
  (define bt-dir-add (new button% [parent bt-dir-panel]
                          [label "Add"]
                          [callback (λ(bt ev)(add-directory))]))
  (define bt-dir-remove
    (new button% [parent bt-dir-panel]
         [label "Remove"]
         [callback (λ(bt ev)
                     (define dir (send dir-lb get-string-selection))
                     (when dir
                       (remove-directory dir)
                       (send files-lb clear)))]))
  (define bt-dir-panel2 (new horizontal-panel% [parent dir-panel]
                             [stretchable-height #f]))

  (define files-panel (new vertical-panel% [parent panels]))
  (define files-lb
    (new list-box% [parent files-panel]
         [label "Scripts"]
         [choices '()]
         [style '(extended vertical-label)]
         [callback
          (λ(lb ev)
            (case (send ev get-event-type)
              [(list-box-dclick)
               (ex/include-selected-file)]
              [else
               (define dir (send dir-lb get-string-selection))
               (when dir
                 (define-values (checked? file)
                   (files-lb-selection-values))
                 (when file
                   (send bt-files-un/check set-label
                         (if checked? "Exclude" "Include"))))]))]))

  (define bt-files-panel (new horizontal-panel% [parent files-panel]
                              [stretchable-height #f]
                              [alignment '(center center)]))
  (define bt-files-un/check
    (new button% [parent bt-files-panel]
         [label "Exclude"]
         [callback
          (λ(bt ev)(ex/include-selected-file))]))

  (define lib-panel (new horizontal-panel% [parent fr]
                         [stretchable-height #f]
                         [alignment '(center center)]))
  (define bt-save-lib (new button% [parent lib-panel]
                           [label "Save&&Exit"]
                           [callback (λ(bt ev)
                                       (lib:save! the-lib the-lib-file)
                                       (send fr show #f))]))
  (define bt-cancel (new button% [parent lib-panel]
                         [label "Cancel"]
                         [callback (λ(bt ev)(send fr show #f))]))

  (send fr show #t))

(module+ main
  (make-library-gui))
