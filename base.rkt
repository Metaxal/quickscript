#lang at-exp racket/base
(require racket/path
         racket/dict)

(provide (all-defined-out))

(module+ test
  (require rackunit))

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

;; proc-name : string?
;; label : string?
(define (make-simple-script-string proc-name label)
  @string-append{
#lang racket/base
(require quickscript/script)

;; See the manual in the Scripts>Manage Scripts>Help menu for more information.

(define-script @proc-name
  #:label "@label"
  (λ(selection)
    #f))
})

;; script-filename : path-string?
(define (make-submod-path script-filename)
  (list 'submod
        (list 'file (path-string->string script-filename))
        'script-info))

;; script-filename : path-string?
;; Returns #f or a string.
(define (get-script-help-string script-filename)
  (dynamic-require (make-submod-path script-filename)
                   'quickscript-module-help-string
                   (λ()#f)))

;; Returns a list of dictionaries of the properties of the scripts in script-filename.
;; Important: Loads the file in the current namespace, so a new namespace should probably
;; be created with (make-base-empty-namespace).
;; script-filename : path-string?
(define (get-property-dicts script-filename)
  (define the-submod (make-submod-path script-filename))
  (dynamic-require the-submod #f)
  (define-values (vars syntaxes) (module->exports the-submod))
  (define funs (map car (dict-ref vars 0)))
  (define property-dicts
    (for/list ([fun (in-list funs)])
        (cons fun (dynamic-require the-submod fun))))
  property-dicts)

(module+ test
  (require racket/file)
  (define dir (find-system-path 'temp-dir))
  (define filename "tmp-script.rkt")
  (define filepath (build-path dir filename))
  (define proc-sym 'my-first-script)
  (define proc-name (symbol->string proc-sym))
  (define label "My First Script")
  (display-to-file (make-simple-script-string proc-name label)
                   filepath
                   #:exists 'replace)

  ; Note: because the script requires `quickscript/script`,
  ; quickscript must be installed as package/collection for the following to work.
  (define prop-dicts
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (get-property-dicts filepath)))
  (check = (length prop-dicts) 1)
  (define props (cdr (car prop-dicts)))
  (define loaded-proc-sym (car (car prop-dicts)))
  (check string=?
         (dict-ref props 'label)
         label)
  (check eq?
         proc-sym
         loaded-proc-sym)
  )
