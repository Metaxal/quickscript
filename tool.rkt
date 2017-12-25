#lang at-exp racket/base

(require
  (for-syntax racket/base) ; for help menu
  compiler/compiler
  drracket/tool ; necessary to build a drracket plugin
  framework ; for preferences (too heavy a package?)
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
  )
(provide tool@)

#|
To debug:
$ export PLTSTEDDR=debug@quickscript && drracket&

If the menu takes a long time to load, it's because the scripts are not compiled.
Compile them with `raco make *.rkt` in the scripts directory, then the menu should load blazingly
fast.

|#

(define-logger quickscript)

(define-runtime-path bundled-scripts-path
  (build-path "bundled-scripts"))

(define base-default-user-script-dir (find-system-path 'pref-dir))

(preferences:set-default 'user-script-dir
                         (path->string (build-path base-default-user-script-dir
                                                   "user-scripts"))
                         path-string?)

(define (script-dir)
  (preferences:get 'user-script-dir))

(log-quickscript-info "Using script-directory: ~a" (script-dir))

; Copy sample scripts at installation (or if user's script directory does not exist):
(unless (directory-exists? (script-dir))
  (make-directory* base-default-user-script-dir)
  ;(message-box "copy scripts" "The scripts are being copied to your user directory")
  (copy-directory/files bundled-scripts-path (script-dir)))

(define (set-script-dir dir)
  (preferences:set 'user-script-dir (if (path? dir) (path->string dir) dir)))

(define (choose-script-dir)
  (let ([d (get-directory "Choose a directory to store scripts" #f
                          (script-dir))])
    (when d (set-script-dir d))))

(define (error-message-box filename e)
  (message-box filename
               (format "Error in script file ~s: ~a" filename (exn-message e))
               #f '(stop ok)))

(define (user-script-files)
  (filter (λ(f)(equal? (path-get-extension f) #".rkt"))
          (directory-list (script-dir) #:build? #t)))

(define (compile-user-scripts)
  (define my-compiler (compile-zos #f #:module? #t))
  (my-compiler (user-script-files) 'auto))

(define-namespace-anchor a)

;; the preference panel is automatically added by DrRacket (nice feature!)
(preferences:add-panel
 "Scripts"
 (λ(parent)
   (define pref-panel (new vertical-panel% [parent parent]
                           [alignment     '(center center)]
                           [spacing       10]
                           [horiz-margin  10]
                           [vert-margin   10]
                           ))
   (define dir-panel (new horizontal-panel% [parent pref-panel]))
   (define text-dir (new text-field% [parent dir-panel]
                         [label       "Script directory:"]
                         [init-value  (script-dir)]
                         [enabled     #f]))
   (new button% [parent dir-panel]
        [label     "Change script directory"]
        [callback  (λ _ (choose-script-dir))])
   (preferences:add-callback 'user-script-dir
                             (λ(p v)(send text-dir set-value v)))
   pref-panel))

(define-syntax-rule (time-info str body ...)
  (let ([ms (current-milliseconds)])
    (log-quickscript-info (string-append "Begin: " str "..."))
    (begin0
      (begin body ...)
      (log-quickscript-info
       (string-append "End  : " str ". Took " (number->string (- (current-milliseconds) ms)) "ms")))))

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

        (define (get-the-text-editor)
          ; for a frame:text% :
          ;(define text (send frame get-editor))
          ; for DrRacket:
          (define defed (get-definitions-text))
          (if (send defed has-focus?)
              defed
              (get-interactions-text)))

        (define frame this)

        (define props-default
          `((label . "My Script 1") ; Should be mandatory
            (menu-path . ())
            (shortcut . #f)
            (shortcut-prefix . #f)
            (help-string . "My amazing script")
            (output-to . selection) ; outputs the result in a new tab
            (persistent? . #f)
            ))

        (define (prop-dict-ref props key)
          (dict-ref props key (dict-ref props-default key)))

        (define (new-script)
          (define name (get-text-from-user "Script name" "Enter the name of the new script:"
                                           #:validate non-empty-string?))
          (when name
            (define filename (string-append (string-foldcase (string-replace name " " "-")) ".rkt"))
            (define file-path (build-path (script-dir) filename))
            (define proc-name (string-foldcase (string-replace name " " "-")))
            (define label name)

            (with-output-to-file file-path
              (λ _
                (displayln @string-append{
 #lang racket/base
 (require quickscript/utils)

 ;; See the manual in the Scripts/Help menu for more information.

 (define-script @proc-name
   #:label "@label"
   (λ(selection)
     #f))
 })))
            (reload-scripts-menu)
            (edit-script file-path)))

        ;; file: path?
        (define (edit-script file)
          (when file
            ; For frame:text% :
            ;(send (get-the-text-editor) load-file file)
            ; For DrRacket:
            (send this open-in-new-tab file)))

        (define (open-script)
          (define file (get-file "Open a script" frame (script-dir) #f #f '()
                                 '(("Racket" "*.rkt"))))
          (edit-script file))

        ;; Ask the user for a script to import from the bundled script directory
        ;; (or any other directory for that matter).
        ;; Useful when new scripts have been added due to an update.
        (define (import-bundled-script)
          (define src-file (get-file "Open a script" frame bundled-scripts-path #f #f '()
                                     '(("Racket" "*.rkt"))))
          (when src-file
            (define src-dir   (path-only src-file))
            (define filename  (path->string (file-name-from-path src-file)))
            (define dest-file (build-path (script-dir) filename))

            (define overwrite? (or (not (file-exists? dest-file))
                                   (eq? 'ok
                                        (message-box
                                         "Overwrite?"
                                         (string-append
                                          "The script " filename
                                          " already exists in your script directory.\n"
                                          "Do you want to overwrite it?")
                                         frame
                                         '(caution ok-cancel)))))
            (when overwrite?
              (copy-file src-file  dest-file  #t))))

        ;; dict for persistent scripts:
        ;; the module is instanciated only once, and made available for future calls.
        (define namespace-dict (make-hash))

        (define (unload-persistent-scripts)
          (set! namespace-dict (make-hash)))

        ;; f: path?
        (define (run-script fun file output-to persistent?)
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
            (with-handlers ([exn:fail? (λ(e)(error-message-box
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
                    (let ([k-v (sort (map (λ(k)(assoc k kw-dict)) kws)
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
               (insert-to-text (get-the-text-editor))] ; get the newly created text
              [(selection)
               (insert-to-text text)]
              [(message-box)
               (when (string? str-out)
                 (message-box "Ouput" str-out this))]
              [(clipboard)
               (when (string? str-out)
                 (send the-clipboard set-clipboard-string str-out 0))]
              )))

        (define (open-help)
          (send-main-page #:sub "quickscript/index.html"))

        (define (bug-report)
          (send-url "https://github.com/Metaxal/quickscript/issues"))

        (define menu-bar (send this get-menu-bar))

        (define menu-reload-count 0)

        (define scripts-menu
          (new menu% [parent menu-bar] [label "&Scripts"]))

        (define (reload-scripts-menu)
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
          ;; add script items:
          ;for all scripts in the script directory:
          (parameterize ([current-namespace (make-base-empty-namespace)])
            (for ([f (in-list (user-script-files))])
              (time-info
               (string-append "Loading file " (path->string f))
               ; catch problems and display them in a message-box
               (with-handlers ([exn:fail? (λ(e)(error-message-box
                                                (path->string (file-name-from-path f))
                                                e))])
                 (define the-submod (list 'submod (list 'file (path->string f)) 'script-info))
                 (define property-dicts
                   (let ()
                     (time-info
                      "Dynamic require"
                      (dynamic-require the-submod #f))
                     (define-values (vars syntaxes) (module->exports the-submod))
                     (define funs (map car (dict-ref vars 0)))
                     (for/list ([fun (in-list funs)])
                       (cons fun (time-info
                                  "Dynamic require fun"
                                  (dynamic-require the-submod fun))))))
                 (for ([(fun props) (in-dict property-dicts)])
                   (let*([label                (prop-dict-ref  props 'label)]
                         [menu-path            (prop-dict-ref  props 'menu-path)]
                         [shortcut             (prop-dict-ref  props 'shortcut)]
                         [shortcut-prefix (or  (prop-dict-ref  props 'shortcut-prefix)
                                               (get-default-shortcut-prefix))]
                         [help-string          (prop-dict-ref  props 'help-string)]
                         [output-to            (prop-dict-ref  props 'output-to)]
                         [persistent?          (prop-dict-ref  props 'persistent?)]
                         )
                     ; Create the menu hierarchy if it doesn't exist.
                     (define parent-menu
                       (let loop ([menu-path menu-path] [parent scripts-menu])
                         (if (empty? menu-path)
                             parent
                             (let ([menu (first menu-path)])
                               (loop (rest menu-path)
                                     (or (findf (λ(m)(and (is-a? m labelled-menu-item<%>)
                                                          (string=? (send m get-label) menu)))
                                                (send parent get-items))
                                         (new menu% [parent parent] [label menu])))))))
                     (new menu-item% [parent parent-menu]
                          [label            label]
                          [shortcut         shortcut]
                          [shortcut-prefix  shortcut-prefix]
                          [help-string      help-string]
                          [callback         (λ(it ev)
                                              (run-script fun
                                                          f
                                                          output-to
                                                          persistent?))])))))))))

        (define manage-menu (new menu% [parent scripts-menu] [label "&Manage scripts"]))
        (for ([(lbl cbk) (in-dict `(("&New script..."              . ,new-script)
                                    ("&Open script..."             . ,open-script)
                                    ("&Import bundled script..."   . ,import-bundled-script)
                                    (separator                     . #f)
                                    ("&Reload scripts menu"        . ,reload-scripts-menu)
                                    ("&Unload persistent scripts"  . ,unload-persistent-scripts)
                                    ("&Compile user scripts"       . ,compile-user-scripts)
                                    (separator                     . #f)
                                    ("&Help"                       . ,open-help)
                                    ("&Feedback/Bug report..."     . ,bug-report)
                                    ))])
          (if (eq? lbl 'separator)
              (new separator-menu-item% [parent manage-menu])
              (new menu-item% [parent manage-menu] [label lbl]
                   [callback (λ _ (cbk))])))
        (new separator-menu-item% [parent scripts-menu])

        (reload-scripts-menu)
        ))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame script-menu-mixin)

    ))
