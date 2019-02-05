#lang at-exp racket/base
(require
  (for-syntax racket/base) ; for help menu
  drracket/tool ; necessary to build a drracket plugin
  #;framework ; for preferences (too heavy a package?)
  help/search
  net/sendurl ; for the help menu
  racket/class
  racket/dict
  racket/file
  racket/gui/base
  racket/list
  racket/path ; for filename-extension
  racket/runtime-path ; for the help menu
  racket/string
  racket/unit
  "base.rkt"
  (prefix-in lib: "library.rkt")
  "library-gui.rkt"
  )
(provide tool@)

#|
To debug:
$ export PLTSTEDDR=debug@quickscript && drracket&

If the menu takes a long time to load, it's because the scripts are not compiled.
Click on Scripts|Manage scripts|Compile user scripts.
It should then be very fast to load.

|#

(define (user-script-files #:exclude? [exclude? #t])
  (lib:all-files (lib:load library-file) #:exclude? exclude?))

(define (error-message-box filename e)
  (message-box "Quickscript caught an exception" #;filename
               (format "Error in script file ~s: ~a" filename (exn-message e))
               #f '(stop ok)))

;; Recompiles all (enabled or disabled, user and third-party) scripts that are not yet compiled
;; for the current version.
;; This is to prevent Quickscript trying to load from compiled after an upgrade of
;; the Racket system, and displaying one error message for each script.
;; It is important to recompile disabled scripts too, because these may still be
;; dependencies of shadowing scripts.
;; Caveat: Dependencies are not compiled automatically. Hence if a script depends on a collection
;; (package) then the collection needs to be compiled with the correct version, otherwise
;; an error will be raised on DrRacket startup.
;; How to test this works:
;; - Create a new script or use an old one that is *not* deactivated in the library
;; - Compile it with another version of racket (install locally, not unix style,
;; then use its old raco to setup quickscript and make the script)
;; - In DrRacket, use the quickscript "Compiled version" to make sure it has the old version.
;; - Exit DrRacket.
;; - Use the new version of raco to setup (again) quickscript
;;   If installing a new version of racket, it may be necessary to run:
;;   $ raco pkg update --link <path-to-local-quickscript>
;; - Restart DrRacket, the script should be compiled silently with the correct version,
;;   and no error message should be displayed.
;; - In the library, check that a quickscript-extra shadowed script does not raise an error
;;   when clicking on it.
(define (recompile-all-of-previous-version)
  (compile-user-scripts
   (filter (λ (f) (not (compiled-for-current-version? f)))
           (user-script-files #:exclude? #f))))

(define-namespace-anchor a)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

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

        (define/private (prop-dict-ref props key)
          (dict-ref props key (dict-ref props-default key)))

        (define/private (new-script)
          (define name (get-text-from-user "Script name" "Enter the name of the new script:"
                                           #:validate non-empty-string?))
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
        (define/private (run-script fun file output-to persistent?)
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
                (dict-ref! namespace-dict file make-script-namespace)
                (make-script-namespace)))

          (define file-str (path->string file))
          (define ed-file (send (get-definitions-text) get-filename))
          (define str-out
            (with-handlers ([exn:fail? (λ (e) (error-message-box
                                               (path->string (file-name-from-path file))
                                               e)
                                         #f)])
              ; See HelpDesk for "Manipulating namespaces"
              (parameterize ([current-namespace ns])
                (let ([f (dynamic-require file fun)]
                      [kw-dict `((#:definitions   . ,(get-definitions-text))
                                 (#:interactions  . ,(get-interactions-text))
                                 (#:editor        . ,text)
                                 (#:file          . ,ed-file)
                                 (#:frame         . ,this))])
                  (let-values ([(_ kws) (procedure-keywords f)])
                    (let ([k-v (sort (map (λ (k) (assoc k kw-dict)) kws)
                                     keyword<? #:key car)])
                      (keyword-apply f (map car k-v) (map cdr k-v) str '())))))))
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
          ;; remove all scripts items, after the persistent ones:
          (time-info
           "Deleting menu items"
           (for ([item (list-tail (send scripts-menu get-items) 2)])
             (log-quickscript-info "Deleting menu item ~a... " (send item get-label))
             (send item delete)))
          
          ;; Add script items.
          ;; Create an empty namespace to load all the scripts (in the same namespace)
          (parameterize ([current-namespace (make-base-empty-namespace)])
            ;; For all scripts in the script directory.
            (for ([f (in-list (user-script-files))])
              (time-info
               (string-append "Loading file " (path->string f))
               ; catch problems and display them in a message-box
               (with-handlers ([exn:fail? (λ (e) (error-message-box
                                                  (path->string (file-name-from-path f))
                                                  e))])
                 (define property-dicts (get-property-dicts f))
                 (for ([(fun props) (in-dict property-dicts)])
                   (let*([label           (prop-dict-ref props 'label)]
                         [menu-path       (prop-dict-ref props 'menu-path)]
                         [shortcut        (prop-dict-ref props 'shortcut)]
                         [shortcut-prefix (or (prop-dict-ref props 'shortcut-prefix)
                                              (get-default-shortcut-prefix))]
                         [help-string     (prop-dict-ref props 'help-string)]
                         [output-to       (prop-dict-ref props 'output-to)]
                         [persistent?     (prop-dict-ref props 'persistent?)]
                         [os-types        (prop-dict-ref props 'os-types)]
                         )
                     (when (memq this-os-type os-types)
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
                            [callback         (λ (it ev) 
                                                (run-script fun
                                                            f
                                                            output-to
                                                            persistent?))]))))))))))

        (define manage-menu (new menu% [parent scripts-menu] [label "&Manage scripts"]))
        (for ([(lbl cbk)
               (in-dict
                `(("&New script..."             . ,(λ () (new-script)))
                  ("&Open script..."            . ,(λ () (open-script)))
                  (separator                    . #f)
                  ("&Library"                   . ,(λ () (make-library-gui #:parent-frame this
                                                                         #:drracket-parent? #t)))
                  ("&Reload menu"                . ,(λ () (reload-scripts-menu)))
                  ("&Compile scripts and reload" . ,(λ () 
                                                      (compile-user-scripts (user-script-files))
                                                      (reload-scripts-menu)))
                  ("&Unload persistent scripts" . ,(λ () (unload-persistent-scripts)))
                  (separator                    . #f)
                  ("&Help"                      . ,(λ () (open-help)))
                  ("&Feedback/Bug report..."    . ,(λ () (bug-report)))
                  ))])
          (if (eq? lbl 'separator)
              (new separator-menu-item% [parent manage-menu])
              (new menu-item% [parent manage-menu] [label lbl]
                   [callback (λ _ (cbk))])))
        (new separator-menu-item% [parent scripts-menu])

        ; Silently recompile for the new version if necessary, at the start up of DrRacket.
        (recompile-all-of-previous-version)
        (reload-scripts-menu)
        ))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame script-menu-mixin)

    ))
