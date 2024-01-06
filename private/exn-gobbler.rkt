#lang racket/base
(require racket/string
         racket/list
         racket/match
         racket/contract)

(provide
 (struct-out exn-gobbler)
 (contract-out
  [make-exn-gobbler     ([] [(or/c string? #false)] . ->* . exn-gobbler?)]
  [exn-gobbler-empty?   (exn-gobbler? . -> . boolean?)]
  [gobble               (exn-gobbler? exn? string? . -> . void?)]
  [exn-gobbler->string  (exn-gobbler? . -> . string?)]))

(struct exn-gobbler (title strs summaries)
  #:mutable
  #:transparent)

(define (make-exn-gobbler [title #f])
  (exn-gobbler title '() '()))

(define (exn-gobbler-empty? gb)
  (empty? (exn-gobbler-strs gb)))

(define (exn->string exn)
    (define sp (open-output-string))
    (parameterize ([current-error-port sp])
      ((error-display-handler) (exn-message exn) exn))
    (get-output-string sp))

(define (gobble gobbler exn summary)
  
  #;(define marks
    (continuation-mark-set->context (exn-continuation-marks exn)))
  #;(define str
    (string-join
     (filter-map (λ (m)
                   (match m
                     [(cons v (srcloc orig line column pos span))
                      (format "~a:~a:~a"orig line column)]
                     [else #false]))
                 marks)
     "\n"))
  (define str (exn->string exn))

  (set-exn-gobbler-strs!      gobbler (cons str     (exn-gobbler-strs gobbler)))
  (set-exn-gobbler-summaries! gobbler (cons summary (exn-gobbler-summaries gobbler))))

(define (exn-gobbler->string gobbler)
  (define summaries (reverse (exn-gobbler-summaries gobbler)))
  (define strs      (reverse (exn-gobbler-strs      gobbler)))
  (string-append
   (if (exn-gobbler-title gobbler)
     (string-append (exn-gobbler-title gobbler) "\n")
     "")
   (format "~a error(s) have been caught.\n" (length summaries))
   "\nSummary:\n"
   (string-join summaries "\n")
   "\n\nDetails:\n"
   "------------------------\n"
   (string-join (map (λ (su st) (string-append su "\n\n" st)) summaries strs)
                "\n------------------------\n")))

;; Usage example
(module+ main
  (define gb (make-exn-gobbler))
  
  (for ([n '(0 1 0 2 3 0)]
        [i (in-naturals)])
    (with-handlers* ([exn:fail? (λ (e) (gobble gb e (format "Error at iteration ~a" i)))])
      (/ 1 n)))
  
  (displayln (exn-gobbler->string gb)))
