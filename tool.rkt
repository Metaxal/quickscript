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
  "private/base.rkt"
  "private/exn-gobbler.rkt"
  (prefix-in lib: "private/library.rkt")
  "private/library-gui.rkt")
(provide tool@)

#|
To debug:
$ export PLTSTDERR=debug@quickscript && drracket&

If the menu takes a long time to load, it's because the scripts are not compiled.
Click on `Scripts|Manage scripts|Compile scripts and reload`.
It should then be very fast to load.

The maximize button of the frame also disappears, as if the X11 maximize property was gone

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

(define (property-dict-hook? props)
  (not (prop-dict-ref props 'label)))

(define (text-insert text str)
  ; Inserts the text, possibly overwriting the selection:
  (send text begin-edit-sequence)
  (send text insert str)
  (send text end-edit-sequence))


(define-namespace-anchor a)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (set! orig-display-handler drracket:init:original-error-display-handler)

    ;===================;
    ;=== Frame mixin ===;
    ;===================;

    (define script-menu-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-interactions-text
                 ;register-toolbar-button
                 create-new-tab)

        (define/private (get-the-text-editor)
          ; for a frame:text% :
          ;(define text (send frame get-editor))
          ; for DrRacket:
          (define defed (get-definitions-text))
          (if (send defed has-focus?)
              defed
              (get-interactions-text)))

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
          (define file (get-file "Open a script" this user-script-dir #f #f '()
                                 '(("Racket" "*.rkt"))))
          (edit-script file))


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

        ;::::::::::::::::;
        ;:: Run Script ::;
        ;::::::::::::::::;

        ;; dict for persistent scripts:
        ;; the module is instanciated only once, and made available for future calls.
        (define namespace-dict (make-hash))

        (define/private (unload-persistent-scripts)
          (set! namespace-dict (make-hash)))

        ;; f: path?
        (define/private (run-script props
                                    #:tab [tab #f]
                                    #:editor [editor #f]
                                    #:more-kwargs [more-kwargs '()])
          (define name         (prop-dict-ref props 'name))
          (define fpath        (prop-dict-ref props 'filepath))
          (define output-to    (prop-dict-ref props 'output-to))
          (define persistent?  (prop-dict-ref props 'persistent?))
          (define hook?        (property-dict-hook? props))
          ; For frame:text% :
          ;(define text (send frame get-editor))
          ; For DrRacket:
          (set! tab (or tab (send this get-current-tab)))
          (set! editor
            (or editor
                (if hook?
                  (send tab get-defs)
                  (get-the-text-editor))))
          (define defs (send tab get-defs))
          (define ints (send tab get-ints))
          (define str (and (not hook?) ; avoid unnecessary computation
                           (send editor get-text
                                 (send editor get-start-position)
                                 (send editor get-end-position))))

          ; Create a namespace for the script:
          (define (make-script-namespace)
            (define ns (make-base-empty-namespace))
            (for ([mod '(racket/class racket/gui/base drracket/tool-lib)])
              (namespace-attach-module (namespace-anchor->empty-namespace a)
                                       mod ns))
            ns)
          ; if the script is persistent, we try to load an existing namespace, or we create one.
          ; if not, we always create a new namespace.
          (define ns
            (if persistent?
                (dict-ref! namespace-dict fpath make-script-namespace)
                (make-script-namespace)))

          (define script-result
            (with-error-message-box
                (format "Run: Error in script file ~s:\n" (path->string fpath))
              #:error-value #f

              ; See HelpDesk for "Manipulating namespaces"
              (let ([f (parameterize ([current-namespace ns])
                         ; Ensure the script is compiled for the correct version of Racket
                         (compile-user-script fpath)
                         (dynamic-require fpath name))]
                    [kw-dict
                     (append
                      `((#:definitions   . ,defs)
                        (#:interactions  . ,ints)
                        (#:editor        . ,editor)
                        (#:file          . ,(send defs get-filename))
                        (#:frame         . ,this))
                      more-kwargs)])
                ;; f is applied *outside* the created namespace so as to make
                ;; all features of drracket's frame available.
                ;; If it were evaluated inside ns, (send fr open-in-new-tab <some-file>)
                ;; wouldn't work.
                (let-values ([(_ kws) (procedure-keywords f)])
                  (let ([k-v (sort (filter-map (λ (k) (assoc k kw-dict)) kws)
                                   keyword<? #:key car)])
                    (if hook?
                      (keyword-apply f (map car k-v) (map cdr k-v) '())
                      (keyword-apply f (map car k-v) (map cdr k-v) str '())))))))
          (cond [hook?
                 ;; Return the script result as is, to be used by the caller.
                 ;; However, since multiple hooks may be called, there's usually no good way
                 ;; to aggregate the return values, so we just return void
                 script-result]
                [(or (string? script-result) (is-a? script-result snip%))
                 ;; Do not modify the file if no output
                 (case output-to
                   [(new-tab)
                    (create-new-tab)
                    (define new-defs (get-definitions-text))
                    (send new-defs select-all) ; get the newly created text
                    (text-insert new-defs script-result)]
                   [(selection)
                    (text-insert editor script-result)]
                   [(message-box)
                    (when (string? script-result)
                      (message-box "Output" script-result this))]
                   [(clipboard)
                    (when (string? script-result)
                      (send the-clipboard set-clipboard-string script-result 0))])]))

        ;; Runs *all* scripts with the given name (identifier) that are *not* menu items.
        ;; TODO: Rename #:label to #:menu-label or #:entry-label for clarity?
        ;; Notice: the result is #<void>, and the results from the scripts are discarded.
        (define/public (find-and-run-hook-scripts name
                                                  #:tab [tab #f]
                                                  #:editor [editor #f]
                                                  #:more-kwargs [more-kwargs '()])
          (for ([props (in-list (hash-ref property-dicts name '()))])
            (run-script props #:tab tab #:editor editor #:more-kwargs more-kwargs)))

        ;::::::::::::::::;
        ;:: Some hooks ::;
        ;::::::::::::::::;

        ;; NOTICE: If new define/pubment methods are added in drracket/private/unit.rkt,
        ;; then these methods must be declared also in drracket/private/interface.rkt

        ;; TODO: Should we have an `after-tab-change`?
        (define/augment (on-tab-change tab-from tab-to)
          (inner (void) on-tab-change tab-from tab-to)
          (queue-callback
           (λ () (find-and-run-hook-scripts 'on-tab-change
                                            #:more-kwargs `((#:tab-from . ,tab-from)
                                                            (#:tab-to   . ,tab-to))))))

        (define/augment (on-close)
          (inner (void) on-close)
          (queue-callback
           (λ () (find-and-run-hook-scripts 'on-close #:more-kwargs '()))))

        ;; At this stage, the frame is not shown yet
        (define/public (on-startup)
          (queue-callback
           (λ () (find-and-run-hook-scripts 'on-startup #:more-kwargs '()))))

        (define/augment (after-create-new-drracket-frame show?)
          (inner (void) after-create-new-drracket-frame show?)
          (queue-callback ; TODO: should this be here or in drracket:unit?
           (λ () (find-and-run-hook-scripts 'after-create-new-drracket-frame
                                                #:more-kwargs `((#:show? . ,show?))))))

        ;; Specialized to only when a new empty tab is created.
        ;; For loading a file, see `on-load-file`.
        ;; If `filename` is not #f, is almost redundant with `on-load-file`, except that the latter
        ;; is also called (I think) when loading a file in an existing tab.
        (define/augment (after-create-new-tab tab filename start-pos end-pos)
          (inner (void) after-create-new-tab tab filename start-pos end-pos)
          (queue-callback
           (λ ()
             ;; #:tab and #:filename are redundant. The same information is already
             ;; available via #:file and #:definitions in the script function.
             (if filename
               (find-and-run-hook-scripts 'after-load-file
                                          #:tab tab
                                          #:more-kwargs `((#:in-new-tab? . #t)))
               (find-and-run-hook-scripts 'after-create-new-tab
                                          #:tab tab
                                          #:more-kwargs `())))))

        ;; TODO: Add a hook for execute-callback. Maybe add a pubment method in drr:unit:frame
        ;; that matches the hook.
        ;;https://docs.racket-lang.org/tools/drracket_unit.html#%28meth._%28%28%28lib._drracket%2Ftool-lib..rkt%29._drracket~3aunit~3aframe~25%29._execute-callback%29%29

        ;::::::::::::::::;
        ;:: Properties ::;
        ;::::::::::::::::;

        ;; All menu item scripts have are at the key `#f` in property-dicts.
        ;; The key for other scripts (hooks, not menu entries) is the script's identifier (name).
        (define property-dicts (make-hasheq))

        (define/private (load-properties!)
          (set! property-dicts (make-hasheq))
          (define gb (make-exn-gobbler "Loading Scripts menu"))
          ;; Create an empty namespace to load all the scripts (in the same namespace).
          (parameterize ([current-namespace (make-base-empty-namespace)]
                         [error-display-handler orig-display-handler])
            ;; For all script files in the script directory.
            (for ([f (in-list (user-script-files))])
              (time-info
               (string-append "Loading file " (path->string f))
               (with-handlers* ([exn:fail?
                                 (λ (e)
                                   (gobble gb e (format "Script file ~s:" (path->string f)))
                                   '())])
                 (define props-list (get-property-dicts f))
                 (for ([props (in-list props-list)])
                   ; Keep only the scripts that match the current os type.
                   (when (memq this-os-type (prop-dict-ref props 'os-types))
                     (define key
                       (if (prop-dict-ref props 'label)   ; TODO: CHECK DEFAULT
                         #f ; This is a menu item script
                         (prop-dict-ref props 'name))) ; This is a hook
                     (hash-update! property-dicts key (λ (acc) (cons props acc)) '())))))))
          ; Don't display an error on menu build, as an error is already shown
          ; during compilation.
          (log-quickscript-info (exn-gobbler->string gb))
          #;(exn-gobbler-message-box gb "Quickscript: Errors while loading script properties"))

        (define/private (reload-scripts-menu)
          (time-info
           "Building script menu"
           (set! menu-reload-count (add1 menu-reload-count))
           (log-quickscript-info "Script menu rebuild #~a..." menu-reload-count)

           (load-properties!)

           (let* ([property-dicts
                   ;; Keep only menu entries
                   (hash-ref property-dicts #f '())]
                  [property-dicts
                   ;; Sort the menu items lexicographically
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
                         #:cache-keys? #t)])

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
                     [help-string     (prop-dict-ref props 'help-string)])
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

        (define manage-menu (new menu% [parent scripts-menu] [label "&Manage"]))
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
                  ("&Compile scripts"            . ,(λ ()
                                                      (unload-persistent-scripts)
                                                      (compile-library/frame)
                                                      (reload-scripts-menu)))
                  ("&Stop persistent scripts"    . ,(λ () (unload-persistent-scripts)))
                  (separator                     . #f)
                  ("&Help"                       . ,(λ () (open-help)))
                  ("Report an &issue"            . ,(λ () (bug-report)))
                  ))])
          (if (eq? lbl 'separator)
              (new separator-menu-item% [parent manage-menu])
              (new menu-item% [parent manage-menu] [label lbl]
                   [callback (λ _ (cbk))])))
        (new separator-menu-item% [parent scripts-menu])

        ;; Show the error messages that happened during the initial compilation.
        (exn-gobbler-message-box init-compile-exn-gobbler "Quickscript: Error during compilation")

        (reload-scripts-menu)
        (on-startup)))

    ;====================;
    ;=== Editor mixin ===;
    ;====================;

     (define text-mixin
      (mixin ((class->interface text%)) ()

        (define (get-drr-frame)
          (send (send this get-tab) get-frame))

        (define/augment (after-load-file success?)
          ;; If the current definitions is not this, this means the file is loaded
          ;; in a new text. In this case, it it better to let `after-create-new-tab`
          ;; trigger an event, because it happens after the on-tab-change event,
          ;; that is, the #:definitions is the correct one.
          (when (and success?
                     (eq? this (send (get-drr-frame) get-definitions-text)))
            ;; Callback is queue so it happens *after* the file is effectively loaded into the
            ;; editor.
            (queue-callback
             (λ () (send (get-drr-frame) find-and-run-hook-scripts
                         'after-load-file
                         ;#:tab  ; no need because we check above it's always the current one
                         #:more-kwargs `((#:in-new-tab? . #f)))))))

        ;; filename : path?
  	;; format :  (or/c 'guess 'same 'copy 'standard 'text 'text-force-cr)
        (define/augment (on-save-file filename fmt)
          ;; No queue-callback to ensure modifications to the file are performed immediately.
          (send (get-drr-frame) find-and-run-hook-scripts
                'on-save-file
                #:tab (send this get-tab) ; TODO: What if `this` is interactions?
                #:editor this
                #:more-kwargs `((#:save-filename . ,filename)
                                (#:format . ,fmt))))

        (define/augment (after-save-file success?)
          (when success?
            (queue-callback
             (λ () (send (get-drr-frame) find-and-run-hook-scripts
                         'after-save-file
                         #:tab (send this get-tab) ; TODO: What if `this` is interactions?
                         #:editor this
                         #:more-kwargs `())))))

        #;(define/override (on-focus on?) (super on-focus on?) #f)

        #;(define/augment (on-insert start len) (inner (void) on-insert start len) #f)
        #;(define/augment (after-insert start len) (inner (void) after-insert start len) #f)

        #;(define/augment (on-delete start len) (inner (void) on-delete start len) #f)
        #;(define/augment (after-delete start len) (inner (void) after-delete start len) #f)

        #;(define/override (on-default-char kw-evt) (super on-defeault-char kw-evt) #f)
        #;(define/override (on-default-event ms-evt) (super on-default-event ms-evt) #f)

        ;; TODO maybe:
        #;can-load-file? #;can-save-file?
        #;on-edit-sequence #;after-edit-sequence
        #;on-scroll-to #;after-scroll-to
        #;on-change
        #;on-display-size
        #;on-snip-modified

        (super-new)))

     ;=================;
     ;=== Tab mixin ===;
     ;=================;

     (define tab-mixin
       (mixin (drracket:unit:tab<%>) ()
         (inherit get-frame)
         (define/augment (on-close)
           (inner (void) on-close)
           (send (get-frame) find-and-run-hook-scripts
                 'on-tab-close
                 #:tab this
                 #:more-kwargs `((#:tab . ,this))))
         (super-new)))

    ; If an exception is raised during these two phases, DrRacket displays
    ; the error in a message box and deactivates the plugin before continuing.
    (define (phase1) (void))
    (define (phase2) (void))


    ; Silently recompile for the new version if necessary, at the start up of DrRacket.
    ; This must be done before building the menus.
    ; The compilation is done at this point so that the splash screen doesn't disappear,
    ; but the message box will be shown after the DrRacket frame is shown up.
    (define init-compile-exn-gobbler (compile-library))

    ;; Search for "Extending the Existing DrRacket Classes" to see what can be extended:
    (drracket:get/extend:extend-definitions-text text-mixin)
    (drracket:get/extend:extend-tab              tab-mixin)
    (drracket:get/extend:extend-unit-frame       script-menu-mixin)))
