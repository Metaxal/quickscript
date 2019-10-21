#lang at-exp racket/base
(require racket/dict
         racket/format
         racket/path
         compiler/compilation-path         
         compiler/compiler
         )

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define version-bytes (string->bytes/utf-8 (version)))

(define-logger quickscript)

(define quickscript-dir
  (build-path (find-system-path 'pref-dir) "quickscript"))

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
  `((label . "My Script 1") ; Should be mandatory
    (menu-path . ())
    (shortcut . #f)
    (shortcut-prefix . #f) ; should be (get-default-shortcut-prefix), but this depends on gui/base
    (help-string . "My amazing script")
    (output-to . selection) ; outputs the result in a new tab
    (persistent? . #f)
    (os-types . (unix macosx windows)) ; list of supported os types
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

;; Returns a list of dictionaries of the properties of the scripts in script-filename.
;; IMPORTANT: Loads the file in the current namespace, so a new namespace should probably
;; be created with (make-base-empty-namespace).
;; script-filename : path-string?
(define (get-property-dicts script-filepath)
  (define the-submod (make-submod-path script-filepath))
  (dynamic-require the-submod #f)
  (define-values (vars syntaxes) (module->exports the-submod))
  (define funs (map car (dict-ref vars 0)))
  (define property-dicts
    (filter values
            (for/list ([fun (in-list funs)])
              (define maybe-props (dynamic-require the-submod fun))
              (and (property-dict? maybe-props)
                   (cons fun maybe-props)))))
  property-dicts)

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
  (define props (cdr (car prop-dicts)))
  (define proc-sym2 (car (car prop-dicts)))
  (check string=?
         (dict-ref props 'label)
         label)
  (check eq?
         proc-sym
         proc-sym2)
  (check string=?
         help-str
         help-str2)
  )

;===================;
;=== Compilation ===;
;===================;

(define (compile-user-scripts files)
  ; Docs say generates a compiled file in the "compiled" directory
  ; (thus not in the "compile d/errortrace" directory).
  (define my-compiler (compile-zos #f #:module? #t))
  (time-info
   "Compiling user scripts"
   (my-compiler files 'auto)))

; Based on 'read-linklet-bundle-or-directory':
; https://github.com/racket/racket/blob/master/racket/src/expander/compile/read-linklet.rkt#L9
; and 'get-cached-compiled':
; https://github.com/racket/racket/blob/master/racket/src/expander/run/cache.rkt#L76
(define (zo-version source-file)
  ; We (only) use "compiled" as modes, because by default DrRacket would place zos in
  ; compiled/errortrace, but the compile-zos used in compile-user-scripts places them in
  ; "compiled".
  (define zo-file (get-compilation-bytecode-file source-file #:modes '("compiled")))
  (and (file-exists? zo-file)
       (parameterize ([read-accept-compiled #t])
         (call-with-input-file*
             zo-file
           (lambda (in)
             (read-bytes 2 in) ; consume "#~"
             (define vers-len (min 63 (read-byte in)))
             (read-bytes vers-len in))))))

;; Is the zo file for the given source file having the same version as
;; the current (dr)racket one?
(define (compiled-for-current-version? source-file)
  (equal? version-bytes (zo-version source-file)))
