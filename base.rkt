#lang racket/base
(require "private/base.rkt"
         (for-syntax racket/base
                     syntax/transformer)
         (prefix-in lib: "private/library.rkt"))
(provide get-script-help-string
         script-file?
         user-script-dir)
(define-syntax user-script-dir
  (make-variable-like-transformer #'(get-user-script-dir)))
(define (get-user-script-dir)
  (lib:user-script-dir (lib:load)))
