#lang racket/base
(require racket/system
         racket/path
         quickscript)

(script-help-string "Open a terminal in the directory of the current file.")

(define-script open-terminal
  #:label "Open terminal here"
  #:menu-path ("&Utils")
  #:os-types (unix macosx windows)
  (λ (str #:file f)
    (unless f
      (set! f (current-directory)))
    (define dir (path->string (path-only f)))
    (case (system-type 'os)
      [(unix)
       (system (string-append "gnome-terminal"
                              " --working-directory=\"" dir "\""
                              " -t \"" dir "\""
                              "&"))]
      [(macosx)
       (system
        (string-append "osascript -e 'tell app \"Terminal\" to do script \"cd \\\"" dir "\\\"\"'" ))]
      [(windows)
       (shell-execute #f "cmd.exe" "" dir 'sw_shownormal)])
    #false))
