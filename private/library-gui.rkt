#lang at-exp racket/base
(require framework
         racket/gui/base
         racket/class
         setup/dirs
         racket/file
         "base.rkt"
         (prefix-in lib: "library.rkt")
         "shadow-script.rkt"
         )

(provide make-library-gui)

(define (check+file->un/checked-file checked? f)
  (string-append-immutable (if checked? "☑ " "☐ ")
                           f))

(define data-list-box%
  (class list-box%
    (init [(d->s datum->string)]
          [choices '()])
    (define datum->string d->s)
    (super-new [choices '()])
    (set choices)
    (inherit append clear get-data get-number get-selection set-selection)
    (define/override (set choices)
      (clear)
      (for ([d (in-list choices)])
        (append (datum->string d) d)))
    (define/public (set-datum-selection d)
      (unless (for/first ([i (in-range (get-number))]
                          #:when (equal? d (get-data i)))
                (set-selection i)
                #t)
        (raise-arguments-error '|set-datum-selection in data-list-box%|
                               "no item matching the given datum"
                               "given" d)))
    (define/public (get-datum-selection)
      (define i (get-selection))
      (and i (get-data i)))))

(define (make-library-gui #:parent-frame [parent-frame #f]
                          #:drracket-parent? [drracket-parent? #f])
  ;; Load the files in a new namespace so that if the file is changed
  ;; the library can pick up the changes.
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (log-quickscript-info "Starting the library GUI.")
    (define the-lib (lib:load))
    (define (save! new-lib)
      (lib:save! new-lib)
      (set! the-lib new-lib))
    (define (user-script-dir)
      (lib:user-script-dir the-lib))

    (define (files-lb-selection-values)
      (define cf (send files-lb get-datum-selection))
      (if cf
          (values (car cf) (cdr cf))
          (values #f #f)))

    (define (set-files-lb dir)
      (send files-lb set (lib:directory->enabled+file the-lib dir)))

    ;; Returns the current selected dir, file and whether it is checked,
    ;; if all have a value, otherwise returns #f for all 3 values.
    (define (get-dir+check+file)
      (define dir (send dir-lb get-datum-selection))
      (if dir
          (let-values ([(checked? file) (files-lb-selection-values)])
            (if file
                (values dir checked? file)
                (values #f #f #f)))
          (values #f #f #f)))

    (define (add-directory [dir #f])
      (let* ([dir (or dir
                      (get-directory "Choose a script directory to add to the library"
                                     fr))]
             [dir (and dir (path->complete-path (path->directory-path dir)))])
        (when dir
          (save! (lib:add-directory the-lib dir))
          (reload-dir-lb)
          (send dir-lb set-datum-selection dir)
          (dir-lb-select dir))))

    (define (remove-directory dir)
      (save! (lib:remove-directory the-lib dir))
      (reload-dir-lb))

    (define (remove-selected-dir)
      (define dir (send dir-lb get-datum-selection))
      (when dir
        (remove-directory dir)
        (send files-lb clear)))

    ;; force: (or/c 'exclude 'include #f)
    (define (ex/include-selected-file [force #f])
      (define-values (dir checked? file) (get-dir+check+file))
      (when file
        (define include?
          (case force
            [(exclude) #f]
            [(include) #t]
            [else (not checked?)]))
        (save! ((if include? lib:include lib:exclude) the-lib dir file))
        (set-files-lb dir)
        ; Restore the previously selected item
        (send files-lb set-datum-selection (cons include? file)))
      (update-bt-files-un/check))

    (define (shadow-selected-file)
      (define-values (dir checked? file) (get-dir+check+file))
      (when file
        (define new-script-path
          (build-path (user-script-dir) file))
        (define proceed?
          (eq? 'yes
               (message-box "Create shadow script?"
                            @string-append{
 This will:

 1) Disable the script file
 @(lib:directory->pretty-string the-lib dir #:file file))

 2) Create a new 'shadow' script file
 @(path->string new-script-path)
 that has the same behaviour as the original script file but can be modified @;
 to change its properties;
 @(if drracket-parent?
      "\n3) Open the shadow script file for editing in DrRacket."
      "")
   
 This allows the original script file to be updated (e.g., if part of a package) @;
 while allowing the user (you) to change its properties without changing the @;
 original file directly.

 Do you want to proceed?
}
                            fr
                            '(caution yes-no))))
        (when proceed?
          (define overwrite?
            (or (not (file-exists? new-script-path))
                (eq? 'ok
                     (message-box "File already exists"
                                  (string-append
                                   "The file "
                                   (path->string new-script-path)
                                   " already exists.\n\n"
                                   "Are you sure you want to overwrite it?")
                                  fr
                                  '(caution ok-cancel)))))
          (when overwrite?
            (display-to-file 
             (let ([pth (build-path dir file)])
               (make-shadow-script pth (lib:path->writable-module-path the-lib pth)))
             new-script-path
             #:exists 'replace)
            (ex/include-selected-file 'exclude)
            (dir-lb-select (user-script-dir))
            (when drracket-parent?
              (send parent-frame open-in-new-tab new-script-path))))))

    (define (dir-lb-select [dir (send dir-lb get-datum-selection)])
      (when dir
        (set-files-lb dir)
        (send dir-lb set-datum-selection dir)
        (send bt-dir-remove enable (lib:removable-directory? the-lib dir))
        (send bt-files-shadow enable (not (equal? dir (user-script-dir))))))

    (define (reload-dir-lb)
      (send dir-lb clear)
      (send dir-lb set (lib:directories the-lib #:sorted? #t)))

    (define (set-msg-help-string dir file)
      (when (and dir file)
        (define filepath (build-path dir file))
        (define str (or (with-handlers* ([exn:fail?
                                          (λ (e) "Warning: Cannot read script help string.")])
                          (get-script-help-string filepath))
                        ""))
        (send msg-help-string set-label str)))

    ;;; Widgets

    (define fr (new frame% [parent parent-frame]
                    [label "Script library"]
                    [width 800] [height 400]
                    [min-width 400] [min-height 100]))
    (define panels (new panel:horizontal-dragable% [parent fr]))

    (define dir-panel (new vertical-panel% [parent panels]
                           [style '(auto-hscroll auto-vscroll)]))
    (define dir-lb (new data-list-box% [parent dir-panel]
                        [label "Directories"]
                        [choices (lib:directories the-lib #:sorted? #t)]
                        [datum->string (λ (dir)
                                         (lib:directory->pretty-string the-lib dir))]
                        [style '(single vertical-label)]
                        [callback (λ (lb ev)  (dir-lb-select))]))

    (define bt-dir-panel (new horizontal-panel% [parent dir-panel]
                              [stretchable-height #f]
                              [alignment '(center center)]))
    (define bt-dir-add (new button% [parent bt-dir-panel]
                            [label "&Add"]
                            [callback (λ (bt ev)  (add-directory))]))
    (define bt-dir-remove (new button% [parent bt-dir-panel]
                               [label "&Remove"]
                               [callback (λ (bt ev)  (remove-selected-dir))]))
  
    (define bt-dir-panel2 (new horizontal-panel% [parent dir-panel]
                               [stretchable-height #f]))

    (define files-panel (new vertical-panel% [parent panels]
                             [style '(auto-hscroll auto-vscroll)]))
    (define files-lb
      (new data-list-box% [parent files-panel]
           [label "Scripts"]
           [choices '()]
           [datum->string
            (λ (x)
              (check+file->un/checked-file (car x) (path->string (cdr x))))]
           [style '(extended vertical-label)]
           [callback
            (λ (lb ev) 
              (case (send ev get-event-type)
                [(list-box-dclick)
                 (ex/include-selected-file)]
                [else
                 (update-bt-files-un/check)]))]))

    (define bt-files-panel (new horizontal-panel% [parent files-panel]
                                [stretchable-height #f]
                                [alignment '(center center)]))
    (define bt-files-un/check
      (new button% [parent bt-files-panel]
           [label "Disa&ble"]
           [callback (λ (bt ev)  (ex/include-selected-file))]))

    (define (update-bt-files-un/check)
      (define-values (dir checked? file)
        (get-dir+check+file))
      (when file
        (send bt-files-un/check set-label
              (if checked? "Disa&ble" "Ena&ble"))
        (set-msg-help-string dir file)))
  
    (define bt-files-shadow
      (new button% [parent bt-files-panel]
           [label "S&hadow"]
           [callback (λ (bt ev)  (shadow-selected-file))]))
  
    (define bt-files-edit #f)
    (when drracket-parent?
      (set! bt-files-edit
            (new button% [parent bt-files-panel]
                 [label "&Edit"]
                 [callback
                  (λ (bt ev) 
                    (define-values (dir checked? file)
                      (get-dir+check+file))
                    (when (and dir file)
                      (send parent-frame open-in-new-tab
                            (build-path dir file))))])))

    (define msg-help-string (new message% [parent fr]
                                 [label ""]
                                 [stretchable-width #f]
                                 [auto-resize #t]))

    (define lib-panel (new horizontal-panel% [parent fr]
                           [stretchable-height #f]
                           [alignment '(center center)]))

    (define bt-close (new button% [parent lib-panel]
                          [label "&Close"]
                          [callback (λ (bt ev) (send fr show #f))]))

    (dir-lb-select (user-script-dir))

    (send fr show #t)))

(module+ main
  (make-library-gui))
