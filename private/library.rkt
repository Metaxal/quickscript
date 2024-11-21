#lang racket/base
(require racket/contract
         racket/file
         racket/path
         racket/set
         racket/serialize
         racket/mutability
         framework/preferences
         pkg/path
         setup/collection-search
         setup/collects
         setup/getinfo
         setup/path-to-relative
         "base.rkt")

(module+ for-test
  (provide (contract-out
            [test-quickscript-dir
             (parameter/c (or/c #f path-string?)
                          (or/c #f complete-directory-path/c))])))

(provide (contract-out
          [library?
           (-> any/c boolean?)]
          [load
           (-> library?)]
          [save!
           (-> library? void?)]
          [library=?
           (-> library? library? boolean?)]
          [directory-path?
           (-> path? boolean?)]
          [directories
           (->* [library?]
                [#:sorted? any/c]
                (listof complete-directory-path/c))]
          [rename library-user-script-dir user-script-dir
                  (-> library? complete-directory-path/c)]
          [directory->enabled+file
           (-> library?
               complete-directory-path/c
               (listof (cons/c boolean? path-element?)))]
          [all-enabled-scripts
           (-> library? (listof (and/c path? complete-path?)))]
          [directory<?
           (-> library? path? path? boolean?)]
          [directory->pretty-string
           (->* [library? path?]
                [#:file (or/c #f path-element?)]
                immutable-string?)]
          [path->writable-module-path
           (-> library?
               (and/c path? complete-path?)
               (and/c (list/c (or/c 'file 'lib) immutable-string?)
                      module-path?))]
          [removable-directory?
           (-> library? complete-directory-path/c boolean?)]
          [library-has-directory?
           (-> library? complete-directory-path/c boolean?)]
          [add-directory
           (->i #:chaperone
                ([lib library?]
                 [dir complete-directory-path/c])
                #:pre (lib dir) (not (library-has-directory? lib dir))
                [_ library?])]
          [remove-directory
           (->i #:chaperone
                ([lib library?]
                 [dir complete-directory-path/c])
                #:pre (lib dir) (removable-directory? lib dir)
                [_ library?])]
          [exclude
           (->i #:chaperone
                ([lib library?]
                 [dir complete-directory-path/c]
                 [_ path-element?])
                #:pre (lib dir) (library-has-directory? lib dir)
                [_ library?])]
          [include ; TODO: removal of exclusions for absent collections (needs different API: no path)
           (->i #:chaperone
                ([lib library?]
                 [dir complete-directory-path/c]
                 [_ path-element?])
                #:pre (lib dir) (library-has-directory? lib dir)
                [_ library?])]))

;; Conceptually, a library encapsulates:
;;   - a set of directories containing script files; and
;;   - a set of script files to *not* include (called exclusions).
;; That is, by default all non-excluded files are included (in particular the new ones).
;;
;; The user-script-dir contains ad-hoc scripts shared across Racket versions
;; and is where new scripts are created by the UI.
;; It is part of the library by definition: we store only a set of file names to exclude.
;;
;; The user may add additional ad-hoc directories (also shared across Racket versions),
;; for which we store a hash table mapping complete paths to sets of file names to exclude.
;; More specifically, keys must syntactically specify directories:
;; this uniformity eases comparison, even though it does not solve the
;; general problem of path “equivalence”, which is complex, potentially filesystem-dependent,
;; and not needed in this context.
;;
;; For scripts installed as part of a Racket package---or, more generally,
;; in a collection---the situation is a bit more complicated.
;; These directories are registered declaratively by including
;; `(define quickscript-directory #t)` in an info.rkt file.
;; Observe that the info.rkt file applies to a specific directory:
;; “collection splicing” means that a given collection may have files in
;; multiple directories, none, all, or perhaps only some of which may
;; be Quickscript directories.
;; Furthermore, package authors expect to be able to change which package
;; supplies a particular module as long as they declare appropriate dependencies
;; to maintain compatibility.
;; Therefore:
;;   - For display to users, we preserve the distinctions among directories
;;     using path->relative-string/library, which includes package information.
;;   - For persistent storage, we represent a collection-based exclusion as a
;;     normalized-lib-module-path?, which will continue to apply regardless of
;;     what package (or even direct collection link) supplies the collection.
;;   - The set of collection-based script directories is already stored as part
;;     of the Racket installation (in the info.rkt files and caches).
;;     We do not store it again with our saved state.

(define (directory-path? x)
  (eq? x (path->directory-path x)))
(define/final-prop path-element-set/c
  (set/c path-element? #:cmp 'equal-always))
(define/final-prop complete-directory-path/c
  (and/c path? complete-path? directory-path?))

;; the library data we save (shared across Racket versions)
(serializable-struct
 ;; can evolve in the future using serializable-struct/versions
 library-data (user-exclusions table collection-exclusions)
 #:guard (struct-guard/c
          path-element-set/c
          (and/c (hash/c #:immutable #t
                         #:flat? #t
                         complete-directory-path/c
                         path-element-set/c)
                 hash-equal-always?)
          (set/c (and/c normalized-lib-module-path?
                        (list/c 'lib immutable-string?))
                 #:cmp 'equal-always))
 #:transparent)
(define (empty-library-data)
  (library-data (setalw) #hashalw() (setalw)))

;; a wrapper with installation info and some caches
;; this is NOT thread-safe, due to hash mutation
(struct library (user-script-dir lib collects-script-dirs setup-cache mp-cache pretty-cache)
  #:transparent)
(define (library-data->library maybe-lib)
  (let* ([quickscript-dir (or (test-quickscript-dir) standard-quickscript-dir)]
         [user-script-dir (path->directory-path (build-path quickscript-dir "user-scripts"))]
         [lib (or maybe-lib (struct-copy
                             library-data (empty-library-data)
                             [user-exclusions (user-exclusions-from-deprecated-library
                                               #:user-script-dir user-script-dir)]))]
         [lib (cond
                [(hash-ref (library-data-table lib) user-script-dir #f)
                 (λ (st)
                   (log-quickscript-error
                    "saved library data contained user-scripts-dir as an extra directory")
                   (struct-copy library-data lib
                                [table (hash-remove (library-data-table lib) user-script-dir)]
                                [user-exclusions (set-union st (library-data-user-exclusions st))]))]
                [else
                 lib])]
         [setup-cache (make-hash)]
         [collects-script-dirs (find-collection-based-script-directories)]
         [collects-scripts-dirs
          (if (test-quickscript-dir)
              (for/setalw ([dir (in-immutable-set collects-script-dirs)]
                           #:when (equal? "quickscript" (path->pkg dir #:cache setup-cache)))
                dir)
              collects-script-dirs)])
    (library user-script-dir
             lib
             collects-scripts-dirs
             setup-cache
             (make-hash)
             (make-hash))))

(define standard-quickscript-dir
  ;; not guaranteed to exist
  (build-path (find-system-path 'pref-dir) "quickscript"))
(define test-quickscript-dir
  ;; #f means we are not currently testing, so use standard-quickscript-dir
  ;; When non-false, this parameter also arranges for (load) to ignore
  ;; collection-based scripts that do not come from the "quickscript" package.
  (make-parameter #f (λ (x)
                       (and x (path->directory-path (path->complete-path x))))))

(define find-collection-based-script-directories
  (let ([absent (gensym)])
    (define (find-collection-based-script-directories)
      (for*/setalw ([dir (find-relevant-directories '(quickscript-directory))]
                    [info (in-value (get-info/full dir))]
                    #:when info
                    [v (in-value (info 'quickscript-directory (λ () absent)))]
                    #:when (cond
                             [(eq? #t v)]
                             [else
                              (unless (eq? absent v)
                                (log-quickscript-error
                                 "~a\n  expected: ~e\n  given: ~e\n  directory: ~e"
                                 "bad value for quickscript-directory in info file"
                                 #t
                                 v
                                 dir))
                              #f]))
        (path->directory-path dir)))
    find-collection-based-script-directories))

(define (user-exclusions-from-deprecated-library #:user-script-dir user-script-dir)
  (define deprecated-library-file
    (build-path user-script-dir 'up "library.rktd"))
  (or (and (file-exists? deprecated-library-file)
           (with-handlers ([exn:fail? (λ (e)
                                        (log-quickscript-error "error importing from ~e: ~v"
                                                               deprecated-library-file
                                                               (exn-message e))
                                        #f)])
             (for/first ([{dir lst} (in-hash (file->value deprecated-library-file))]
                         #:when (equal? user-script-dir
                                        (path->complete-path (path->directory-path dir))))
               (for/setalw ([s (in-list lst)])
                 (string->path-element s)))))
      (setalw)))

;; library-data is stored using the framework/preferences system,
;; which provides help for future changes without breaking compatibility
;; Calling (default-library-data) may consult the filesystem, so do it
;; during (load), not when instantiating this module.
(define pref-key 'plt:quickscript:library)
(preferences:set-default pref-key #f (or/c library-data? #f))
(preferences:set-un/marshall pref-key
                             (λ (x)
                               (with-handlers ([exn:fail? (λ (e) 'corrupt)])
                                 (serialize x)))
                             (λ (x)
                               (with-handlers ([exn:fail? void])
                                 (deserialize x))))
(define (load)
  (library-data->library (preferences:get pref-key)))
(define (save! lib)
  (preferences:set pref-key (library-lib lib)))

(define (library=? a b)
  ;; ignores caches
  (define-syntax-rule (cf fld ...)
    (and (equal-always? (fld a) (fld b)) ...))
  (cf library-user-script-dir
      library-lib
      library-collects-script-dirs))

(define (directory<? lib a b)
  ;; Order:
  ;;   - user-script-dir
  ;;   - ad-hoc directories, sorted by path<?
  ;;   - collection-based directories:
  ;;       * first sorted by package, with non-package collections last
  ;;       * then sorted by directory->pretty-string
  ;;   - unknown paths sorted by path<?
  (define table (library-data-table (library-lib lib)))
  (define collects-dirs (library-collects-script-dirs lib))
  (define user-script-dir (library-user-script-dir lib))
  (cond
    [(equal? a b)
     #f]
    [(equal? a user-script-dir)
     #t]
    [(equal? b user-script-dir)
     #f]
    [(hash-has-key? table a)
     (if (hash-has-key? table b)
         (path<? a b)
         #t)]
    [(hash-has-key? table b)
     #f]
    [(set-member? collects-dirs a)
     (if (set-member? collects-dirs b)
         (let* ([cache (library-setup-cache lib)]
                [a-pkg (path->pkg a #:cache cache)]
                [b-pkg (path->pkg b #:cache cache)])
           (cond
             [(equal? a-pkg b-pkg)
              (string<? (directory->pretty-string lib a)
                        (directory->pretty-string lib b))]
             [(and a-pkg b-pkg)
              (string<? a-pkg b-pkg)]
             [a-pkg
              #t]
             [else
              #f]))
         #t)]
    [(set-member? collects-dirs b)
     #f]
    [else
     (path<? a b)]))

(define (directory->pretty-string lib dir #:file [file #f])
  (define full
    (if file
        (build-path dir file)
        dir))
  (hash-ref!
   (library-pretty-cache lib)
   full
   (λ ()
     (string->immutable-string
      (if (hash-has-key? (library-data-table (library-lib lib)) dir)
          (path->string full)
          (path->relative-string/library full #:cache (library-setup-cache lib)))))))

(define (path->normalized-lib-module-path lib pth)
  (hash-ref!
   (library-mp-cache lib)
   pth
   (λ ()
     (define rslt
       (path->module-path pth #:cache (library-setup-cache lib)))
     (and (normalized-lib-module-path? rslt)
          `(lib ,(string->immutable-string (cadr rslt)))))))

(define (path->writable-module-path lib pth)
  ;; TODO: maybe "read" able, since we mean that we can `write` the path, not write TO the path
  (or (path->normalized-lib-module-path lib pth)
      `(file ,(string->immutable-string (path->string pth)))))

(define (directories lib #:sorted? [sorted? #f])
  (cond
    [sorted?
     (sort (directories lib #:sorted? #f)
           (λ (a b)
             (directory<? lib a b)))]
    [else
     (cons (library-user-script-dir lib)
           (append (hash-keys (library-data-table (library-lib lib)))
                   (set->list (library-collects-script-dirs lib))))]))

(define (directory->enabled+file lib dir)
  (define data (library-lib lib))
  (define enabled?
    (cond
      [(if (equal? dir (library-user-script-dir lib))
           (library-data-user-exclusions data)
           (hash-ref (library-data-table data) dir #f))
       => (λ (excludes)
            (λ (name)
              (not (set-member? excludes name))))]
      [else
       (define excludes (library-data-collection-exclusions data))
       (λ (name)
         (define pth (build-path dir name))
         (not (set-member? excludes (path->normalized-lib-module-path lib pth))))]))
  (for/list ([name (in-list (if (directory-exists? dir)
                                (directory-list dir #:build? #f)
                                '()))]
             #:when (and (script-file? name)
                         (file-exists? (build-path dir name)))) ; i.e., not a directory
    (cons (enabled? name) name)))

(define (all-enabled-scripts lib)
  (for*/list ([dir (in-list (directories lib))]
              [enabled+file (in-list (directory->enabled+file lib dir))]
              #:when (car enabled+file))
    (build-path dir (cdr enabled+file))))

(define (removable-directory? lib dir)
  (hash-has-key? (library-data-table (library-lib lib)) dir))

(define (library-has-directory? lib dir)
  (or (equal? dir (library-user-script-dir lib))
      (removable-directory? lib dir)
      (set-member? (library-collects-script-dirs lib) dir)))

(define (add-directory lib dir)
  (define data (library-lib lib))
  (struct-copy library lib
    [lib (struct-copy library-data data
                      [table (hash-update (library-data-table data)
                                          dir
                                          values
                                          setalw)])]))

(define (remove-directory lib dir)
  (define data (library-lib lib))
  (struct-copy library lib
               [lib (struct-copy library-data data
                                 [table (hash-remove (library-data-table data)
                                                     dir)])]))

(define (in/exclude set-change lib dir filename)
  (define data (library-lib lib))
  (struct-copy library lib
    [lib (cond
           [(equal? dir (library-user-script-dir lib))
            (struct-copy
             library-data data
             [user-exclusions (set-change (library-data-user-exclusions data) filename)])]
           [(set-member? (library-collects-script-dirs lib) dir)
             (struct-copy
              library-data data
              [collection-exclusions
               (set-change (library-data-collection-exclusions data)
                           (path->normalized-lib-module-path lib (build-path dir filename)))])]
           [else
             (struct-copy
              library-data data
              [table (hash-update (library-data-table data)
                                  dir
                                  (λ (excludes)
                                    (set-change excludes filename)))])])]))

(define (exclude lib dir filename)
  (in/exclude set-add lib dir filename))

(define (include lib dir filename)
  (in/exclude set-remove lib dir filename))
