#lang racket/base
(require (for-syntax racket/base
                     racket/runtime-path
                     (only-in quickscript/library add-third-party-script-directory!)))

;; This is going to be called during setup and will automatically
;; register the `scripts` folder in quickscript's library.
(begin-for-syntax
  (define-runtime-path script-dir "scripts")
  (add-third-party-script-directory! script-dir))

;; to be removed
