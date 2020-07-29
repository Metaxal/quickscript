#lang racket
;; Tests n a separate file to test the contracts too.
(require rackunit
         quickscript/base
         quickscript/library)

(define my-lib (new-library))
(check set=?
       (directories my-lib)
       (map path-string->string (list user-script-dir)))
  
(define dummy-dir (build-path (find-system-path 'temp-dir)
                              "dummy-script-dir"))
; Make sure we control what the directory contains.
(make-directory* dummy-dir)
(for-each delete-file (directory-list dummy-dir #:build? #t))

(define lib-path (build-path dummy-dir "library.rktd"))

(set! my-lib (load lib-path))
  
(add-directory! my-lib dummy-dir)
(check set=?
       (directories my-lib)
       (map path-string->string (list user-script-dir dummy-dir)))

  
(define script1 "script1.rkt")
(define script2 "script2.rkt")
(define not-a-script "script.notrkt")
(display-to-file "\n" (build-path dummy-dir script1))
(display-to-file "\n" (build-path dummy-dir script2))
(display-to-file "\n" (build-path dummy-dir not-a-script))
(check set=?
       (files my-lib dummy-dir)
       (list script1 script2))

(exclude! my-lib dummy-dir script1)
(check set=?
       (files my-lib dummy-dir)
       (list script2))
(exclude! my-lib dummy-dir script2)
(check set=?
       (files my-lib dummy-dir)
       '())

(include! my-lib dummy-dir script1)
(check set=?
       (files my-lib dummy-dir)
       (list script1))
  
(remove-directory! my-lib dummy-dir)
(check set=?
       (directories my-lib)
       (map path-string->string (list user-script-dir)))

;; Check load and save!
(add-directory! my-lib dummy-dir)
(exclude! my-lib dummy-dir script2)
(define my-lib-file (build-path dummy-dir "my-lib.rktd"))
(save! my-lib my-lib-file)
(define my-lib2 (load my-lib-file))
(check set=? (dict-keys my-lib) (dict-keys my-lib2))
(for ([(dir excl-list) (in-dict my-lib)])
  (check-equal? excl-list (exclusions my-lib2 dir)))
(check set=?
       (all-files my-lib)
       (all-files my-lib2))
#;(all-files my-lib2)
