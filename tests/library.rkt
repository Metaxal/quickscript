#lang racket
;; Tests in a separate file to test the contracts too.
(require rackunit
         "../private/base.rkt"
         "../private/library.rkt"
         (submod "../private/library.rkt" for-test)
         (only-in pkg/lib pkg-directory)
         (only-in setup/getinfo reset-relevant-directories-state!)
         framework/preferences)

(define-binary-check (check-equal-always? equal-always? actual expected))
(define-binary-check (check-library=? library=? actual expected))
(define-simple-check (check-not-library=? v1 v2)
  (not (library=? v1 v2)))

(define (pe s)
  (string->path-element s))

(define (touch* #:in dir . files)
  (make-directory* dir)
  (for ([f (in-list files)])
    (call-with-output-file* (build-path dir f) void)))

(define prefs-table
  (make-hash))

(define tmp-qs-dir
  (path->directory-path (make-temporary-directory)))

(define-syntax-rule (in-test-context body ...)
  (parameterize ([test-quickscript-dir tmp-qs-dir]
                 [preferences:low-level-get-preference
                  (λ (name [fail (λ () #f)])
                    (hash-ref prefs-table name fail))]
                 [preferences:low-level-put-preferences
                  (λ (names vals)
                    (for ([name (in-list names)]
                          [val (in-list vals)])
                      (hash-set! prefs-table name val)))])
    (preferences:restore-defaults)
    (reset-relevant-directories-state!)
    (dynamic-wind
     void
     (λ ()
       (call-with-continuation-barrier
        (λ ()
          body ...)))
     (λ ()
       (delete-directory/files tmp-qs-dir #:must-exist? #f)))))

(in-test-context

 (define the-user-script-dir
   (build-path tmp-qs-dir "user-scripts/"))

 (define quickscript-pkg-directory
   (cond
     [(pkg-directory "quickscript")
      => simplify-path]
     [else
      (fail-check "tests require the \"quickscript\" package to be installed")]))
 (define the-pkg-script-dir
   (build-path quickscript-pkg-directory "scripts/"))
 (define expected-pkg-enabled+file
   `([#t . ,(pe "eyes.rkt")]
     [#t . ,(pe "open-terminal.rkt")]))

 (define my-lib (load))

 (check-equal? (user-script-dir my-lib)
               the-user-script-dir
               "user-script-dir configured for testing")

 (check-equal-always? (directories my-lib)
                      (list the-user-script-dir
                            the-pkg-script-dir)
                      "default library directories")

 (check-equal-always? (directory->enabled+file my-lib the-user-script-dir)
                      '()
                      "no user script yet")

 (check-equal-always? (directory->enabled+file my-lib the-pkg-script-dir)
                      expected-pkg-enabled+file
                      "expected package scripts")

 (define extra-dir
   (build-path tmp-qs-dir "extra/"))

 (define lib+extra
   (add-directory my-lib extra-dir))

 (check-equal-always? (directories lib+extra)
                      (list the-user-script-dir
                            extra-dir
                            the-pkg-script-dir)
                      "add-directory")

 (check-equal-always? (directory->enabled+file lib+extra extra-dir)
                      '()
                      "extra-dir does not exist yet")

 (define script1 (pe "script1.rkt"))
 (define script2 (pe "script2.rkt"))
 (define not-a-script (pe "script3.notrkt"))
 (touch* #:in extra-dir script1 script2 not-a-script)

 (check-equal-always? (directory->enabled+file lib+extra extra-dir)
                      `([#t . ,script1]
                        [#t . ,script2])
                      "correct scripts in extra directory")

 (define lib+excludes
   (for/fold ([lib lib+extra])
             ([d (list extra-dir
                       the-pkg-script-dir
                       the-user-script-dir)]
              [f (list script1
                       (cdar expected-pkg-enabled+file)
                       (pe "excluded.rkt"))]) ; does not exist yet
     (exclude lib d f)))

 (check-equal-always? (directory->enabled+file lib+excludes the-pkg-script-dir)
                      `([#f . ,(cdar expected-pkg-enabled+file)]
                        ,@(cdr expected-pkg-enabled+file))
                      "package script exclusions")

 (check-equal-always? (directory->enabled+file
                       (include lib+excludes
                                the-pkg-script-dir
                                (cdar expected-pkg-enabled+file))
                       the-pkg-script-dir)
                      expected-pkg-enabled+file
                      "include for package script")

 (check-equal-always? (directory->enabled+file lib+excludes extra-dir)
                      `([#f . ,script1]
                        [#t . ,script2])
                      "extra-dir excludes")

 (check-equal-always? (directory->enabled+file
                       (include (exclude lib+excludes extra-dir script2)
                                extra-dir
                                script1)
                       extra-dir)
                      `([#t . ,script1]
                        [#f . ,script2])
                      "extra-dir: swap includes and excludes")

 (define extra+pkg-enabled-scripts
   (cons (build-path extra-dir script2)
         (for/list ([pr (in-list (cdr expected-pkg-enabled+file))])
           (build-path the-pkg-script-dir (cdr pr)))))

 (check-equal-always? (all-enabled-scripts lib+excludes)
                      extra+pkg-enabled-scripts
                      "expected enabled scripts before user")

 (touch* #:in the-user-script-dir "user.rkt" "excluded.rkt")

 (check-equal-always? (all-enabled-scripts lib+excludes)
                      (cons (build-path the-user-script-dir "user.rkt")
                            extra+pkg-enabled-scripts)
                      "should detect added user script")

 (check-equal-always? (directory->enabled+file lib+excludes the-user-script-dir)
                      `([#f . ,(pe "excluded.rkt")]
                        [#t . ,(pe "user.rkt")])
                      "should recognize pre-excluded user script")

 (check-equal-always? (directory->enabled+file
                       (include lib+excludes the-user-script-dir (pe "excluded.rkt"))
                       the-user-script-dir)
                      `([#t . ,(pe "excluded.rkt")]
                        [#t . ,(pe "user.rkt")])
                      "include user script")

 (delete-file (build-path the-user-script-dir "user.rkt"))

 (check-equal-always? (all-enabled-scripts lib+excludes)
                      extra+pkg-enabled-scripts
                      "should detect deleted user script")

 (check-equal-always? (directories
                       (remove-directory lib+excludes extra-dir))
                      (list the-user-script-dir
                            the-pkg-script-dir)
                      "can remove extra-dir")

 ;; TODO: test correct errors from add remove include exclude

 ;; Check load and save!
 (check-not-library=? my-lib
                      lib+excludes
                      "my-lib is different than lib+excludes")
 (check-library=? (load)
                  my-lib
                  "(load) reproduces my-lib")
 (save! lib+excludes)
 (check-library=? (load)
                  lib+excludes
                  "save and restore lib+excludes"))
