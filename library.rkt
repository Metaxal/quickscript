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
  (make-directory* user-script-dir)
  (write-to-file lib file #:exists 'replace))

(define (directories lib)
  (dict-keys lib))

(define (exclusions lib dir)
  (dict-ref lib (path-string->string dir) '()))

;; Returns the list of script files in the given directory.
;; If exclude is not #f, then only such files that are not listed as exclusions
;; in the library are returned.
(define (files lib [dir user-script-dir] #:exclude? [exclude? #t])
  (define script-files
    (map path->string
         (filter (λ (f) (script-file? (build-path dir f)))
                 (if (directory-exists? dir)
                     (directory-list dir #:build? #f)
                     '()))))
  (cond [exclude?
         (define except-list (exclusions lib dir))
         (set-subtract script-files except-list)]
        [else script-files]))

;; Returns the list full paths of script files --in all listed directories of the library.
;; The keyword argument `exclude?' is as in `files'.
(define (all-files lib #:exclude? [exclude? #t])
  (for*/list ([dir (in-dict-keys lib)]
              [f (in-list (files lib dir #:exclude? exclude?))])
    (build-path dir f)))


(define (add-directory! lib dir [excl '()])
  (dict-ref! lib (path-string->string dir) excl)
  (void))

(define (remove-directory! lib dir)
  (dict-remove! lib (path-string->string dir)))

(define (exclude! lib dir filename)
  (dict-update! lib
                (path-string->string dir)
                (λ (excl) (set-add excl filename))))

(define (include! lib dir filename)
  (dict-update! lib
                (path-string->string dir)
                (λ (excl) (set-remove excl filename))))

(define (add-third-party-script-directory! dir [excl '()])
  (define lib (load))
  (add-directory! lib dir excl)
  (save! lib))

(define (remove-third-party-script-directory! dir)
  (define lib (load))
  (remove-directory! lib dir)
  (save! lib))

(provide/contract
 [library?            (any/c . -> . boolean?)]
 [new-library         (-> library?)]
 [load                ([]
                       [path-string?] ; does not need to exist
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
                       [path-string? #:exclude? boolean?]
                       . ->* . (listof string?))]
 [all-files           ([library?]
                       [#:exclude? boolean?]
                       . ->* . (listof path-string?))]
 [add-directory!      ([library?
                        (and/c path-string? absolute-path? directory-exists?)]
                       [list?]
                       . ->* . void?)]
 [remove-directory!   (library?
                       (and/c path-string? absolute-path?)
                       . -> . void?)]
 [exclude!            (library?
                       (and/c path-string? absolute-path?)
                       (and/c string? path-free?)
                       . -> . void?)]
 [include!            (library?
                       (and/c path-string? absolute-path?)
                       (and/c string? path-free?)
                       . -> . void?)]
 [add-third-party-script-directory!
                      ([(and/c path-string? absolute-path?)]
                       [(listof (and/c string? path-free?))]
                       . ->* . void?)]
 [remove-third-party-script-directory!
                      ((and/c path-string? absolute-path?)
                       . -> . void?)]
 )
