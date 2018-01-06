#lang at-exp racket/base

(require "base.rkt"
         racket/dict
         racket/format
         racket/string)

(provide make-shadow-script)

(define shadow-prefix "shadow:")

(define (make-header f)
@string-append{
#lang racket/base
(require quickscript/script
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
  @(if (dstr 'persistent?)
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
  (define f (build-path (path-only (resolve-module-path 'quickscript-extra))
                         "scripts" "bookmarks.rkt"))
  (displayln (make-shadow-script f)))
