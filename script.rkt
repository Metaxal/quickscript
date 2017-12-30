#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide define-script)

;; Keep this file as light as possible as it is loaded in each script.

(begin-for-syntax
  (define submodule-content '()))

(begin-for-syntax
  (define (add-submod-content! stx)
    (syntax-parse stx
      [(_ body ...)
       (when (null? submodule-content)
         (syntax-local-lift-module-end-declaration
          #'(generate-submodule)))
       (set! submodule-content
             (cons (syntax-local-introduce
                    #`(begin body ...))
                   submodule-content))])))

;; Does not work yet
(provide module-script-info+)
(define-syntax (module-script-info+ stx)
  (add-submod-content! stx)
  #'(void))

(define-syntax (define-script stx)
  (syntax-parse stx
    [(_ proc (~alt (~once (~seq #:label label-val))
                   (~once (~optional (~seq #:menu-path (menu-path-val ...))
                                     #:defaults ([(menu-path-val 1) null])))
                   (~once (~optional (~seq #:help-string help-string-val)
                                     #:defaults ([help-string-val #'""])))
                   (~once (~optional (~seq #:shortcut shortcut-val)
                                     #:defaults ([shortcut-val #'#f])))
                   (~once (~optional (~seq #:shortcut-prefix shortcut-prefix-val)
                                     #:defaults ([shortcut-prefix-val #'#f])))
                   (~once (~optional (~and #:persistent
                                           (~bind [persistent-val #'#t]))
                                     #:defaults ([persistent-val #'#f])))
                   (~once (~optional (~seq #:output-to
                                           (~and output-to-val
                                                 (~or (~datum selection)
                                                      (~datum new-tab)
                                                      (~datum message-box)
                                                      (~datum clipboard))))
                                     #:defaults ([output-to-val #'selection]))))
        ...
        rhs:expr)
     (add-submod-content!
      #`(begin
          (provide proc)
          (define proc (list
                        (cons 'label 'label-val)
                        (cons 'menu-path '(menu-path-val ...))
                        (cons 'help-string 'help-string-val)
                        (cons 'shortcut 'shortcut-val)
                        (cons 'shortcut-prefix 'shortcut-prefix-val)
                        (cons 'persistent? '#,(attribute persistent-val))
                        (cons 'output-to 'output-to-val)))))
     (syntax/loc stx
       (begin (provide proc)
              (define proc rhs)))]))

(define-syntax (generate-submodule stx)
  #`(module script-info racket/base #,@submodule-content)
  ; for debugging:
  #;#`(begin (require racket/pretty)
           (pretty-print (list '#,submodule-content))))


;; The following examples should raise explicit syntax errors
#;(define-script my-script
  ([label "My Script"]
   [menu-path ("a" "b")]
   [persistent? #t]
   [labelito "hey"])
  (λ(str)"yeah"))


#;(define-script my-script
  #:label "My Script2"
  #:menu-path ("a" "b")
  #:help-string "hey"
  #:shortcut 'f9
  #:shortcut-prefixx '(ctl shift)
  ;#:persistent
  (λ(str)"yeah2"))


