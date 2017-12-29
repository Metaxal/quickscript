#lang at-exp racket

(provide make-shadow-script)

(define (make-header f)
@string-append{
#lang racket/base
(require quickscript/script
         (file @(~s (path->string f))))

 })

;; todo: change properties only if not default?
(define (shadow-script-proc fun-sym props)
  #;`(define-script ,(string->symbol (format "shadow:~a" fun-sym))
     #:label ,(dict-ref props 'label)
     #:menu-path ,(dict-ref props 'menu-path)
     #:shortcut ,(dict-ref props 'shortcut)
     #:shortuct-prefix ,(dict-ref props 'shortcut-prefix)
     #:output-to ,(dict-ref props 'output-to)
     ,@(if (dict-ref props 'persistent?)
           '(#:persistent)
           '())
     ,fun-sym)
  (define (dstr sym)
    (~s (dict-ref props sym)))
@string-append{
(define-script @(format "shadow:~a" fun-sym)
  #:label @(dstr 'label)
  #:menu-path @(dstr 'menu-path)
  #:shortcut @(dstr 'shortcut)
  #:shortcut-prefix @(dstr 'shortcut-prefix)
  #:output-to @(dstr 'output-to)
  @(if (dstr 'persistent?)
        "#:persistent"
        "")
  @(~a fun-sym))
  
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
