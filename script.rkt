#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse)

(provide define-script
         define-hook)

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

(provide script-help-string)
(define-syntax (script-help-string stx)
  (syntax-parse stx
    [(_ str:expr)
     (add-submod-content!
      #`(begin
          (provide quickscript-module-help-string)
          (define quickscript-module-help-string 'str)))
     #'(void)]))

(define-syntax (define-script stx)
  (syntax-parse stx
    [(_ proc (~alt (~once (~seq #:label label-val))
                   (~optional (~seq #:menu-path (menu-path-val ...))
                              #:defaults ([(menu-path-val 1) null]))
                   (~optional (~seq #:help-string help-string-val)
                              #:defaults ([help-string-val #'""]))
                   (~optional (~seq #:shortcut shortcut-val)
                              #:defaults ([shortcut-val #'#f]))
                   (~optional (~seq #:shortcut-prefix shortcut-prefix-val)
                              #:defaults ([shortcut-prefix-val #'#f]))
                   (~optional (~and #:persistent
                                    (~bind [persistent-val #'#t]))
                              #:defaults ([persistent-val #'#f]))
                   (~optional (~seq #:output-to
                                    (~and output-to-val
                                          (~or (~datum selection)
                                               (~datum new-tab)
                                               (~datum message-box)
                                               (~datum clipboard)
                                               #f)))
                              #:defaults ([output-to-val #'selection]))
                   (~optional (~seq #:os-types
                                    (~and os-types-val
                                          [(~alt (~optional (~datum unix))
                                                 (~optional (~datum macosx))
                                                 (~optional (~datum windows)))
                                           ...]))
                              #:defaults ([os-types-val #'(unix macosx windows)])))
        ...
        rhs:expr)
     (add-submod-content!
      #`(begin
          (provide proc)
          (define proc (list
                        (cons 'label            'label-val)
                        (cons 'menu-path        '(menu-path-val ...))
                        (cons 'help-string      'help-string-val)
                        (cons 'shortcut         'shortcut-val)
                        (cons 'shortcut-prefix  'shortcut-prefix-val)
                        (cons 'persistent?      '#,(attribute persistent-val))
                        (cons 'output-to        'output-to-val)
                        (cons 'os-types         'os-types-val)))))
     (syntax/loc stx
       (begin (provide proc)
              (define proc rhs)))]))

#;(begin-for-syntax
  (define-splicing-syntax-class maybe-persistent
    (pattern (~optional (~and #:persistent
                              (~bind [persistent? #'#t]))
                        #:defaults ([persistent? #'#f])))))

(begin-for-syntax
  (define known-hook-ids
    '(after-load-file
      on-save-file
      after-save-file
      after-create-new-tab
      on-tab-change
      on-tab-close
      on-startup
      on-close))
  (define known-hook-ids-str
    (apply string-append
           (for*/list ([id known-hook-ids]
                       [s (in-list (list "  " (symbol->string id) "\n"))])
             s))))

(define-syntax (define-hook stx)
  (syntax-parse stx
    [(_ proc:id
        (~alt (~optional (~seq #:help-string help-string-val)
                         #:defaults ([help-string-val #'""]))
              #;persistent:maybe-persistent
              (~optional (~and #:persistent
                               (~bind [persist? #'#t]))
                         #:defaults  ([persist? #'#f]))
              (~optional (~seq #:output-to
                               (~and output-to-val
                                     (~or (~datum selection)
                                          (~datum new-tab)
                                          (~datum message-box)
                                          (~datum clipboard)
                                          #f)))
                         #:defaults ([output-to-val #'selection]))
              (~optional (~seq #:os-types
                               (~and os-types-val
                                     [(~alt (~optional (~datum unix))
                                            (~optional (~datum macosx))
                                            (~optional (~datum windows)))
                                      ...]))
                         #:defaults ([os-types-val #'(unix macosx windows)])))
        ...
        rhs:expr)
     #:fail-when (and (not (memq (syntax-e #'proc)
                                 known-hook-ids))
                      #'proc)
     (string-append "Invalid hook name.\n Valid names:\n" known-hook-ids-str)
     (add-submod-content!
      #`(begin
          (provide proc)
          (define proc (list
                        (cons 'label            #f)
                        (cons 'help-string      'help-string-val)
                        (cons 'persistent?      '#,(attribute persist? #;persistent.persistent?))
                        (cons 'os-types         'os-types-val)))))
     (syntax/loc stx
       (begin (provide proc)
              (define proc rhs)))]))

(define-syntax (generate-submodule stx)
  #`(module script-info racket/base #,@submodule-content)
  ; for debugging: (just Run the script file to observe the generated submodule)
  #;#`(begin (require racket/pretty)
           (pretty-print (list '#,submodule-content))))


;; The following examples should raise explicit syntax errors

#;(define-script my-script
  #:label "My Script2"
  #:menu-path ("a" "b")
  #:help-string "hey"
  #:shortcut 'f9
  #:shortcut-prefixx '(ctl shift)
  ;#:persistent
  (λ (str) "yeah2"))


