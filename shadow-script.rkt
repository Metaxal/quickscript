#lang at-exp racket

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
  @|shadow-prefix|@|fun-str|)
  
}
)

(define (make-shadow-script f)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (define the-submod (list 'submod (list 'file (path->string f)) 'script-info))
    (let ()
      (dynamic-require the-submod #f)
      (define-values (vars syntaxes) (module->exports the-submod))
      (define funs (map car (dict-ref vars 0)))
      (string-append
       (make-header f)
       "\n"
       (string-join
        (for/list ([fun (in-list funs)])
          (shadow-script-proc fun (dynamic-require the-submod fun)))
        "\n")))))


(module+ main
  (define f (string->path
             "/home/orseau/Unison/Prog/Racket/quickscript-extra/scripts/bookmarks.rkt"))
  (displayln (make-shadow-script f)))
