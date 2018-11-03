#lang at-exp racket/base
(require racket/dict
         racket/format
         racket/string
         "base.rkt")

(provide make-shadow-script)

(define shadow-prefix "shadow:")

(define (make-header f)
@string-append{
#lang racket/base
(require quickscript
         (prefix-in @shadow-prefix (file @(~s (path->string f)))))

 })

;; todo: change properties only if not default?
(define (shadow-script-proc fun-sym props)
  (define (dstr sym)
    (~s (dict-ref props sym)))
  (define fun-str (symbol->string fun-sym))
@string-append{
(define-script @fun-str
  #:label @(dstr 'label)
  #:menu-path @(dstr 'menu-path)
  #:shortcut @(dstr 'shortcut)
  #:shortcut-prefix @(dstr 'shortcut-prefix)
  #:output-to @(dstr 'output-to)
  @(if (dict-ref props 'persistent?)
        "#:persistent"
        "")
  @|shadow-prefix|@fun-str)
  
}
)

(define (make-shadow-script f)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (define props-dict (get-property-dicts f))
    (define funs (dict-keys props-dict))
    (string-append
     (make-header f)
     "\n"
     (string-join
      (for/list ([(fun props) (in-dict props-dict)])
        (shadow-script-proc fun props))
      "\n"))))


(module+ main
  (require syntax/modresolve
           racket/path)
  ; don't bother if the module does not exist.
  (with-handlers ([exn:fail:filesystem:missing-module? void])
    (define qs-path (resolve-module-path 'quickscript-extra))
    (define f (build-path (path-only qs-path)
                          "scripts" "bookmarks.rkt"))
    (displayln (make-shadow-script f))))
