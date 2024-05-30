#lang info

(define pkg-name   "quickscript")
(define collection "quickscript")
(define name       "Quickscript")
(define version    "1.1")
(define pkg-desc   "Scripting engine for DrRacket.")
(define license
  '(Apache-2.0 OR MIT))

(define drracket-tools      '(("tool.rkt")))
(define drracket-tool-names '("Quickscript"))
(define drracket-tool-icons '(#f))

(define scribblings
  '(("scribblings/quickscript.scrbl" () (drracket-plugin) "quickscript")))

(define deps
  '(["base" #:version "8.13.0.2"] ; for path equal-always? fix
    "drracket-plugin-lib"
    "gui-lib"
    "net-lib"
    "scribble-lib"))

(define build-deps
  '("at-exp-lib"
    "drracket"
    "gui-doc"
    "racket-doc"
    "rackunit-lib"))
