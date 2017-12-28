#lang racket/base
(require racket/contract
         racket/dict
         racket/file
         racket/set
         "base.rkt"
         )

;; A library is a hash where a key is a directory (as a string)
;; and a value is a list of files (string without path) to *not* include (called exclusions).
;; That is, by default all non-excluded files are included (in particular the new ones).
(define (new-library)
  (define lib (make-hash))
  (add-directory! lib (path->string user-script-dir))
  lib)

(define (library? lib)
  (hash? lib))

(define (load [file library-file])
  (if (file-exists? file)
      (hash-copy (file->value file))
      (new-library)))

(define (save! lib [file library-file])
  (write-to-file lib file #:exists 'replace))

(define (directories lib)
  (dict-keys lib))

(define (exclusions lib dir)
  (dict-ref lib (path-string->string dir) '()))

;; Returns the list of script files in the given directory that are not listed as exclusions
;; in the library.
(define (files lib [dir user-script-dir])
  (define script-files
    (map path->string
         (filter (λ(f)(script-file? (build-path dir f)))
                 (directory-list dir #:build? #f))))
  (define except-list (exclusions lib dir))
  (set-subtract script-files except-list))

(define (all-files lib)
  (for*/list ([dir (in-dict-keys lib)]
              [f (in-list (files lib dir))])
    (build-path dir f)))


(define (add-directory! lib dir [exclusions '()])
  (dict-ref! lib (path-string->string dir) exclusions)
  (void))

(define (remove-directory! lib dir)
  (dict-remove! lib (path-string->string dir)))

(define (exclude! lib dir filename)
  (dict-update! lib
                (path-string->string dir)
                (λ(excl)(set-add excl filename))))

(define (include! lib dir filename)
  (dict-update! lib
                (path-string->string dir)
                (λ(excl)(set-remove excl filename))))

(provide/contract
 [library?            (any/c . -> . boolean?)]
 [load                ([]
                       [(and/c path-string? file-exists?)]
                       . ->* . library?)]
 [save!               ([library?]
                       [path-string?]
                       . ->* . void?)]
 [directories         (library?
                       . -> . (listof string?))]
 [exclusions          (library?
                       path-string?
                       . -> . (listof string?))]
 [files               ([library?]
                       [path-string?]
                       . ->* . (listof string?))]
 [all-files           (library?
                       . -> . (listof path-string?))]
 [add-directory!      ([library?
                        (and/c path-string? absolute-path? directory-exists?)]
                       [list?]
                       . ->* . void?)]
 [remove-directory!   (library?
                       (and/c path-string? absolute-path?)
                       . -> . void?)]
 [exclude!      (library?
                       (and/c path-string? absolute-path?)
                       (and/c string? path-free?)
                       . -> . void?)]
 [include!   (library?
                       (and/c path-string? absolute-path?)
                       (and/c string? path-free?)
                       . -> . void?)]
 )


(module+ test
  (require rackunit)

  (define my-lib (new-library))
  (check set=?
         (directories my-lib)
         (map path-string->string (list user-script-dir)))
  
  (define dummy-dir (build-path quickscript-dir "dummy-script-dir"))
  ; Make sure we control what the directory contains.
  (make-directory* dummy-dir)
  (for-each delete-file (directory-list dummy-dir #:build? #t))
  
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
  )
