#lang racket/base

;; This module provides limited backwards compatibility for packages
;; that followed the old, broken recommendations for registering scripts.
;; The Quickscript library is now actually implemented in "private/library.rkt".

(require "private/base.rkt")

(provide add-third-party-script-directory!
         remove-third-party-script-directory!)

(define (add-third-party-script-directory! dir [excl '()])
  (log-quickscript-error "add-third-party-script-directory! is deprecated and has no effect"))

(define (remove-third-party-script-directory! dir)
  (log-quickscript-error "remove-third-party-script-directory! is deprecated and has no effect"))
