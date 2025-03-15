#lang racket/base

(require racket/class
         racket/gui/base
         string-constants)

(provide (all-defined-out))

;;; Some utilities for scripts

;; Opens a file in a new tab and returns whether opening was successful.
;; Checks if the file exists and displays a message box otherwise and returns #f.
;; Opens the file in the first tab if drracket is still-untouched?
;; Changes to the corresponding tab if the file is already open.
(define (smart-open-file drfr f)
  (cond
    [(not (file-exists? f))
     (message-box (string-constant error)
                  (format (string-constant qs-file-not-found) f)
                  drfr
                  '(ok stop))
     #f]
    [(send drfr still-untouched?)
     (send drfr change-to-file f)
     #t]
    [(send drfr find-matching-tab f)
     =>
     (λ (tab)
       (send drfr change-to-tab tab)
       #t)]
    [else
     (send drfr open-in-new-tab f)
     #t]))