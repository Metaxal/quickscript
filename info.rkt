#lang info

(define deps
  '("base"
    "drracket-plugin-lib"
    "gui-lib"
    "net-lib"
    "scribble-lib"))

(define build-deps
  '("at-exp-lib"
    "drracket"
    "gui-doc"
    "racket-doc"
    "draw-doc"
    "rackunit-lib"))

(define name                 "Quickscript")
(define drracket-tools       '(("tool.rkt")))
(define drracket-tool-names  '("Quickscript"))
(define drracket-tool-icons  '(#f))

(define scribblings '(("scribblings/quickscript.scrbl" () (tool) "quickscript")))

(define compile-omit-paths
  '())
