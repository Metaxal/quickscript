#lang at-exp racket/base
(require
  (for-syntax racket/base) ; for help menu
  drracket/tool ; necessary to build a drracket plugin
  framework ; for preferences (too heavy a package?)
  help/search
  net/sendurl ; for the help menu
  racket/class
  racket/dict
  racket/file
  racket/gui/base
  racket/list
  racket/string
  racket/unit
  "base.rkt"
  "exn-gobbler.rkt"
  (prefix-in lib: "library.rkt")
  "library-gui.rkt")
(provide tool@)

#|
To debug:
$ export PLTSTDERR=debug@quickscript && drracket&

If the menu takes a long time to load, it's because the scripts are not compiled.
Click on `Scripts|Manage scripts|Compile scripts and reload`.
It should then be very fast to load.

|#

(define orig-display-handler #f) ; will be set in the unit.

(define (user-script-files #:exclude? [exclude? #t])
  (lib:all-files (lib:load library-file) #:exclude? exclude?))

(define (error-message-box str e)
  (define sp (open-output-string))
  (parameterize ([current-error-port sp])
    (orig-display-handler (exn-message e) e))

  (message-box "Quickscript caught an exception"
               (string-append str " " (get-output-string sp))
               #f '(stop ok)))

(define-syntax with-error-message-box
  (syntax-rules ()
    [(_ str #:error-value err-val body ...)
     (with-handlers* ([(λ (e) (and (exn? e) (not (exn:break? e))))
                       (λ (e)
                         (error-message-box str e)
                         err-val)])
       body ...)]
    [(_ str body ...)
     (with-error-message-box str #:error-value (void) body ...)]))

;; Shows a message box with the string of the exn-gobbler, if not empty.
(define (exn-gobbler-message-box gb title)
  (unless (exn-gobbler-empty? gb)
    (message-box title
                 (exn-gobbler->string gb)
                 #f
                 '(caution ok))))

;; -> exn-gobbler?
(define (compile-library)
  (time-info "Recompiling library"
               (parameterize ([error-display-handler orig-display-handler])
                 (compile-user-scripts (user-script-files)))))

;; -> void?
(define (compile-library/frame)
  (define fr #false)
    (dynamic-wind
     (λ ()
       (set! fr (new frame% [parent #f] [label "Recompiling quickscripts…"] [width 200] [height 50]))
       (void (new message% [parent fr] [label "Recompiling quickscripts, please wait…"]))
       (send fr reflow-container)
       (send fr show #true))
     (λ ()
       (define gb (compile-library))
       (exn-gobbler-message-box gb "Quickscript: Error during compilation"))
     (λ () (send fr show #false))))

(define-namespace-anchor a)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (set! orig-display-handler drracket:init:original-error-display-handler)

    (define script-menu-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-interactions-text
                 ;register-toolbar-button
                 create-new-tab
                 )

        (define/private (get-the-text-editor)
          ; for a frame:text% :
          ;(define text (send frame get-editor))
          ; for DrRacket:
          (define defed (get-definitions-text))
          (if (send defed has-focus?)
              defed
              (get-interactions-text)))

        (define frame this)

        (define/private (new-script)
          (define name (get-text-from-user "Script name" "Enter the name of the new script:"
                                           this
                                           #:validate non-empty-string?
                                           #:dialog-mixin frame:focus-table-mixin))
          (when name
            (define filename (string-append (string-foldcase (string-replace name " " "-")) ".rkt"))
            (define file-path (build-path user-script-dir filename))
            (define proc-name (string-foldcase (string-replace name " " "-")))
            (define label name)

            ;; Make sure the directory containing the script exists
            (make-directory* user-script-dir)
            ;; Write the script to file
            (with-output-to-file file-path
              (λ _
                (displayln (make-simple-script-string proc-name label))))
            (reload-scripts-menu)
            (edit-script file-path)))

        ;; file: path?
        (define/private (edit-script file)
          (when file
            ; For frame:text% :
            ;(send (get-the-text-editor) load-file file)
            ; For DrRacket:
            (send this open-in-new-tab file)))

        (define/private (open-script)
          (define file (get-file "Open a script" frame user-script-dir #f #f '()
                                 '(("Racket" "*.rkt"))))
          (edit-script file))

        ;; dict for persistent scripts:
        ;; the module is instanciated only once, and made available for future calls.
        (define namespace-dict (make-hash))

        (define/private (unload-persistent-scripts)
          (set! namespace-dict (make-hash)))

        ;; f: path?
        (define/private (run-script props)
          (define name         (prop-dict-ref props 'name))
          (define fpath        (prop-dict-ref props 'filepath))
          (define output-to    (prop-dict-ref props 'output-to))
          (define persistent?  (prop-dict-ref props 'persistent?))
          ; For frame:text% :
          ;(define text (send frame get-editor))
          ; For DrRacket:
          (define text (get-the-text-editor))
          (define str (send text get-text
                            (send text get-start-position)
                            (send text get-end-position)))
          ; Create a namespace for the script:
          (define (make-script-namespace)
            (define ns (make-base-empty-namespace))
            (for ([mod '(racket/class racket/gui/base)])
              (namespace-attach-module (namespace-anchor->empty-namespace a)
                                       mod ns))
            ns)
          ; if the script is persistent, we try to load an existing namespace, or we create one.
          ; if not, we always create a new namespace.
          (define ns
            (if persistent?
                (dict-ref! namespace-dict fpath make-script-namespace)
                (make-script-namespace)))

          (define file-str (path->string fpath))
          (define ed-file (send (get-definitions-text) get-filename))
          (define str-out
            (with-error-message-box
                (format "Run: Error in script file ~s:\n" file-str)
              #:error-value #f
              
              ; See HelpDesk for "Manipulating namespaces"
              (let ([f (parameterize ([current-namespace ns])
                         ; Ensure the script is compiled for the correct version of Racket
                         (compile-user-script fpath)
                         (dynamic-require fpath name))]
                    [kw-dict `((#:definitions   . ,(get-definitions-text))
                               (#:interactions  . ,(get-interactions-text))
                               (#:editor        . ,text)
                               (#:file          . ,ed-file)
                               (#:frame         . ,this))])
                ;; f is applied *outside* the created namespace so as to make
                ;; all features of drracket's frame available.
                ;; If it were evaluated inside ns, (send fr open-in-new-tab <some-file>)
                ;; wouldn't work.
                (let-values ([(_ kws) (procedure-keywords f)])
                  (let ([k-v (sort (map (λ (k) (assoc k kw-dict)) kws)
                                   keyword<? #:key car)])
                    (keyword-apply f (map car k-v) (map cdr k-v) str '()))))))
          (define (insert-to-text text)
            ; Inserts the text, possibly overwriting the selection:
            (send text begin-edit-sequence)
            (send text insert str-out)
            (send text end-edit-sequence))
          ; DrRacket specific:
          (when (or (string? str-out) (is-a? str-out snip%)) ; do not modify the file if no output
            (case output-to
              [(new-tab)
               (create-new-tab)
               (define new-defs (get-definitions-text))
               (send new-defs select-all) ; get the newly created text
               (insert-to-text new-defs)]
              [(selection)
               (insert-to-text text)]
              [(message-box)
               (when (string? str-out)
                 (message-box "Output" str-out this))]
              [(clipboard)
               (when (string? str-out)
                 (send the-clipboard set-clipboard-string str-out 0))]
              )))

        (define/private (open-help)
          (perform-search "quickscript")
          ; Does not seem to work well.
          #;(send-main-page #:sub "quickscript/index.html"))

        (define/private (bug-report)
          (send-url "https://github.com/Metaxal/quickscript/issues"))

        (define menu-bar (send this get-menu-bar))

        (define menu-reload-count 0)

        (define scripts-menu
          (new menu% [parent menu-bar] [label "&Scripts"]))

        (define/private (reload-scripts-menu)
          (time-info
           "Building script menu"
           (set! menu-reload-count (add1 menu-reload-count))
           (log-quickscript-info "Script menu rebuild #~a..." menu-reload-count)

           (let ()
             (define gb (make-exn-gobbler "Loading Scripts menu"))
             ;; Create an empty namespace to load all the scripts (in the same namespace).
             (define property-dicts
               (parameterize ([current-namespace (make-base-empty-namespace)]
                              [error-display-handler orig-display-handler])
                 ;; For all scripts in the script directory.
                 (append-map
                  (λ (f)
                    (time-info
                     (string-append "Loading file " (path->string f))
                     (with-handlers* ([exn:fail?
                                       (λ (e)
                                         (gobble gb e (format "Script file ~s:" (path->string f)))
                                         '())])
                       (define props-list (get-property-dicts f))
                       ; Keep only the scripts that match the current os type.
                       (filter (λ (props) (memq this-os-type (prop-dict-ref props 'os-types)))
                               props-list))))
                  (user-script-files))))
             ; Don't display an error on menu build, as an error is already shown
             ; during compilation .
             (log-quickscript-info (exn-gobbler->string gb))
             #;(exn-gobbler-message-box gb "Quickscript: Errors while loading script properties")
             
             ;; Sort the menu items lexicographically.
             (set! property-dicts
                   (sort property-dicts
                         string<=?
                         #:key (λ (props)
                                 (define menu-path (prop-dict-ref props 'menu-path))
                                 (string-downcase
                                  (string-replace
                                   (string-join
                                    (append
                                     (if (empty? menu-path)
                                       '("/")
                                       menu-path)
                                     (list (prop-dict-ref props 'label)))
                                    "/")
                                   "&" "" #:all? #t)))
                         #:cache-keys? #t))
             ;; remove all scripts items, after the default ones:
             (time-info
              "Deleting menu items"
              (for ([item (list-tail (send scripts-menu get-items) 2)])
                (log-quickscript-info "Deleting menu item ~a... " (send item get-label))
                (send item delete)))
             ;; Add script items.
             (for ([props (in-list property-dicts)])
               (let*([label           (prop-dict-ref props 'label)]
                     [menu-path       (prop-dict-ref props 'menu-path)]
                     [shortcut        (prop-dict-ref props 'shortcut)]
                     [shortcut-prefix (or (prop-dict-ref props 'shortcut-prefix)
                                          (get-default-shortcut-prefix))]
                     [help-string     (prop-dict-ref props 'help-string)]
                     )
                 ; Create the menu hierarchy if it doesn't exist.
                 (define parent-menu
                   (let loop ([menu-path menu-path] [parent scripts-menu])
                     (if (empty? menu-path)
                       parent
                       (let ([menu (first menu-path)])
                         (loop (rest menu-path)
                               (or (findf (λ (m) (and (is-a? m labelled-menu-item<%>)
                                                      (string=? (send m get-label) menu)))
                                          (send parent get-items))
                                   (new menu% [parent parent] [label menu])))))))
                 (new menu-item% [parent parent-menu]
                      [label            label]
                      [shortcut         shortcut]
                      [shortcut-prefix  shortcut-prefix]
                      [help-string      help-string]
                      [callback         (λ (it ev) (run-script props))]))))))

        (define manage-menu (new menu% [parent scripts-menu] [label "&Manage scripts"]))
        (for ([(lbl cbk)
               (in-dict
                `(("&New script…"                . ,(λ () (new-script)))
                  ("&Open script…"               . ,(λ () (open-script)))
                  ("&Disable scripts…"           . ,(λ () (make-library-gui #:parent-frame this
                                                                            #:drracket-parent? #t)))
                  (separator                     . #f)
                  ("&Library…"                   . ,(λ () (make-library-gui #:parent-frame this
                                                                            #:drracket-parent? #t)))
                  ("&Reload menu"                . ,(λ ()
                                                      (unload-persistent-scripts)
                                                      (reload-scripts-menu)))
                  ("&Compile scripts and reload" . ,(λ ()
                                                      (unload-persistent-scripts)
                                                      (compile-library/frame)
                                                      (reload-scripts-menu)))
                  ("&Unload persistent scripts"  . ,(λ () (unload-persistent-scripts)))
                  (separator                     . #f)
                  ("&Help"                       . ,(λ () (open-help)))
                  ("&Feedback/Bug report…"       . ,(λ () (bug-report)))
                  ))])
          (if (eq? lbl 'separator)
              (new separator-menu-item% [parent manage-menu])
              (new menu-item% [parent manage-menu] [label lbl]
                   [callback (λ _ (cbk))])))
        (new separator-menu-item% [parent scripts-menu])

        ;; Show the error messages that happened during the initial compilation.
        (exn-gobbler-message-box init-compile-exn-gobbler "Quickscript: Error during compilation")

        (reload-scripts-menu)))

    ; If an exception is raised during these two phases, DrRacket displays 
    ; the error in a message box and deactivates the plugin before continuing. 
    (define (phase1) (void))
    (define (phase2) (void))

    ; Silently recompile for the new version if necessary, at the start up of DrRacket.
    ; This must be done before building the menus.
    ; The compilation is done at this point so that the splash screen doesn't disappear,
    ; but the message box will be shown after the DrRacket frame is shown up.    
    (define init-compile-exn-gobbler (compile-library))

    (drracket:get/extend:extend-unit-frame script-menu-mixin)))
