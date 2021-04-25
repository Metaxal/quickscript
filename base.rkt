#lang at-exp racket/base
(require racket/contract
         racket/dict
         racket/format
         racket/path
         compiler/compilation-path         
         compiler/cm
         "exn-gobbler.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define version-bytes (string->bytes/utf-8 (version)))
(define vm-bytes (string->bytes/utf-8 (symbol->string (system-type 'vm))))

(define-logger quickscript)

;; TODO: What if (find-system-path 'pref-dir) does not exist?
(define quickscript-dir
  (or (getenv "PLTQUICKSCRIPTDIR")
      (build-path (find-system-path 'pref-dir) "quickscript")))

(define library-file
  (build-path quickscript-dir "library.rktd"))

(define user-script-dir
  (build-path quickscript-dir "user-scripts"))

(define (path-free? p-str)
  (not (path-only p-str)))

(define (path-string->string p-str)
  (if (string? p-str)
      p-str
      (path->string p-str)))

(define (script-file? f)
  (equal? (path-get-extension f) #".rkt"))

(define (path-string=? dir1 dir2)
  (string=? (path-string->string dir1)
            (path-string->string dir2)))

(module+ test

  (check-true (path-free? "a.rkt"))
  (check-false (path-free? "b/a.rkt"))
  (check-false (path-free? "b/a"))
  (when (eq? (system-path-convention-type) 'unix)
    (check-true
     (path-string=? "a/b/c.rkt"
                    (build-path "a" "b/c.rkt"))))
  )

(define-syntax-rule (time-info str body ...)
  (let ([ms (current-milliseconds)])
    (log-quickscript-info (string-append "Begin: " str "..."))
    (begin0
      (begin body ...)
      (log-quickscript-info
       (string-append "End  : " str ". Took " (number->string (- (current-milliseconds) ms)) "ms")))))

(define props-default
  `((name             . #f)
    (filepath         . #f)
    (label            . "My Script 1") ; Should be mandatory
    (menu-path        . ())
    (shortcut         . #f)
    (shortcut-prefix  . #f) ; should be (get-default-shortcut-prefix), but this depends on gui/base
    (help-string      . "My amazing script")
    (output-to        . selection) ; outputs the result in a new tab
    (persistent?      . #f)
    (os-types         . (unix macosx windows)) ; list of supported os types
    ))

(define this-os-type (system-type 'os))

;; proc-name : string?
;; label : string?
;; TODO: extend this with a given property-dict
(define (make-simple-script-string proc-name label
                                   #:script-help-string [script-help-string #f])
  ;; See the manual in the Scripts|Manage Scripts|Help menu for more information.
  @string-append{
#lang racket/base

(require quickscript)
@(if script-help-string (string-append "\n(script-help-string " (~s script-help-string) ")\n") "")
;; Returns a replacement string for the selected string `selection`
;; ("" if no text is selected), or `#f` to leave the selection as is.
(define-script @proc-name
  #:label "@label"
  (λ (selection) 
    #f))
})

;; script-filename : path-string?
(define (make-submod-path script-filename)
  (list 'submod
        (list 'file (path-string->string script-filename))
        'script-info))

;; script-filename : path-string?
;; Returns #f or a string.
;; Important: see note for get-property-dicts
(define (get-script-help-string script-filename)
  (dynamic-require (make-submod-path script-filename)
                   'quickscript-module-help-string
                   (λ () #f)))

(define (property-dict? v)
  (and (dict? v)
       (dict-has-key? v 'label)))

;; Returns a list of dictionaries of the properties of the scripts in script-filename,
;; augmented with the scripts' function and the script filepath.
;; IMPORTANT: Loads the file in the current namespace, so a new namespace should probably
;; be created with (make-base-empty-namespace).
;; script-filename : path-string?
(define (get-property-dicts script-filepath)
  ; Ensure the script is compiled for the correct version of Racket
  (compile-user-script script-filepath)
  
  (define the-submod (make-submod-path script-filepath))
  (dynamic-require the-submod #f)
  (define-values (vars syntaxes) (module->exports the-submod))
  (define funs (map car (dict-ref vars 0)))
  (define property-dicts
    (filter values
            (for/list ([fun (in-list funs)])
              (define maybe-props (dynamic-require the-submod fun))
              (and (property-dict? maybe-props)
                   (list*
                    (cons 'name fun)
                    (cons 'filepath script-filepath)
                    maybe-props)))))
  property-dicts)

(define (prop-dict-ref props key)
  (dict-ref props key (dict-ref props-default key)))

(module+ test
  (require racket/file)
  (define dir (find-system-path 'temp-dir))
  (define filename "tmp-script.rkt")
  (define filepath (build-path dir filename))
  (define proc-sym 'my-first-script)
  (define proc-name (symbol->string proc-sym))
  (define label "My First Script")
  (define help-str "The help-string of the script.")
  (display-to-file (make-simple-script-string proc-name label
                                              #:script-help-string help-str)
                   filepath
                   #:exists 'replace)

  ; Note: because the script requires `quickscript/script`,
  ; quickscript must be installed as package/collection for the following to work.
  (define-values (prop-dicts help-str2)
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (values (get-property-dicts filepath)
              (get-script-help-string filepath))))
  (check = (length prop-dicts) 1)
  (define props (car prop-dicts))
  (check string=?
         (dict-ref props 'label)
         label)
  (check eq?
         (prop-dict-ref props 'name)
         proc-sym)
  (check string=?
         help-str2
         help-str))

;===================;
;=== Compilation ===;
;===================;

(define/contract (compile-user-script file)
  (-> path-string? any)

  ;; Simple wrapper for now, but may be specialized for efficiency later.
  (void)
  #;(compile-user-scripts (list file)))

(define/contract (compile-user-scripts files
                                       #:exn-gobbler [gb (make-exn-gobbler "Compiling scripts")])
  (->* [(listof path-string?)]
       [#:exn-gobbler exn-gobbler?]
       exn-gobbler?)

  ; Synchronous version:
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (define cmc (make-caching-managed-compile-zo))
    (for ([f (in-list files)])
      (with-handlers* ([exn:fail? (λ (e) (gobble gb e (path->string f)))])
        (time-info (format "Compiling ~a" (path->string f))
                   (cmc f)))))
  (log-quickscript-info (exn-gobbler->string gb))
  gb)

(define (zo-file src-file)
  (get-compilation-bytecode-file src-file #:modes '("compiled")))
