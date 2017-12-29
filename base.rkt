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

(define (path-string=? dir1 dir2)
  (string=? (path-string->string dir1)
            (path-string->string dir2)))

(module+ test
  (require rackunit)

  (check-true (path-free? "a.rkt"))
  (check-false (path-free? "b/a.rkt"))
  (check-false (path-free? "b/a"))
  (when (eq? (system-path-convention-type) 'unix)
    (check-true
     (path-string=? "a/b/c.rkt"
                    (build-path "a" "b/c.rkt"))))
  )