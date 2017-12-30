#lang setup/infotab

(define deps
  '("base"
    "at-exp-lib"
    "drracket"
    "drracket-plugin-lib"
    "gui-lib"
    "html-lib"
    "net-lib"
    "slideshow-lib"
    "srfi-lite-lib"
    "gui-doc"
    "racket-doc"
    "racket-index"
    "scribble-lib"
    ))

(define build-deps '("draw-doc" "rackunit-lib"))

(define name                 "Quickscript")
(define drracket-tools       '(("tool.rkt")))
(define drracket-tool-names  '("Quickscript"))
(define drracket-tool-icons  '(#f))

(define scribblings '(("scribblings/quickscript.scrbl" () (tool) "quickscript")))

(define compile-omit-paths
  '())

(define blurb
  '("Easily write scripts for DrRacket."))

(define required-core-version  "6.0")
(define repositories           '("4.x"))
(define categories             '(devtools))

(define can-be-loaded-with  'none)
(define primary-file        "tool.rkt")
