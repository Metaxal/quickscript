#lang racket/base
(require racket/path)

(provide (all-defined-out))


(define-logger quickscript)

(define quickscript-dir
  (build-path (find-system-path 'pref-dir) "quickscript"))

(define library-file
  (build-path quickscript-dir "library.rktd"))

(define user-script-dir
  (build-path quickscript-dir "user-scripts"))

(define (path-free? p-str)
  (not (path-only p-str)))

(define (path-string->string p-str)
  (if (string? p-str)
      p-str
      (path->string p-str)))

(define (script-file? f)
  (equal? (path-get-extension f) #".rkt"))

(module+ test
  (require rackunit)

  (check-true (path-free? "a.rkt"))
  (check-false (path-free? "b/a.rkt"))
  (check-false (path-free? "b/a"))
  )