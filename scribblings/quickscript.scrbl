#lang scribble/manual

@(require racket/file
          racket/path
          racket/runtime-path
          (for-syntax racket/base) ; for build-path in runtime-path
          (for-label racket/gui)
          (for-label drracket/tool-lib))

@(define (codeblock/file file)
   (list
    @(filebox (path->string (file-name-from-path file)) "")
    @(codeblock (file->string file))))

@title{Quickscript, a scripting plugin for DrRacket}

@;author{Laurent Orseau}
@(smaller (author+email "Laurent Orseau" "laurent.orseauREMOVEME@gmail.com" #:obfuscate? #t))

@section{Introduction}

Quickscript's purpose is to make it easy to extend DrRacket with small Racket scripts
that can be used in the definition (or interaction) window, or to graphically interact with the user.

Creating a new script is as easy as a click on a menu item.
Each script is automatically added as an item to the @italic{Scripts} menu, without needing to restart DrRacket.
A keyboard shortcut can be assigned to a script (via the menu item).
By default, a script takes as input the currently selected text, and outputs the replacement text.
There is also direct access to some elements of DrRacket GUI for advanced scripting,
like DrRacket's frame and the definition or interaction editor.

@section{Installation}

To install, either look for @tt{quickscript} in the DrRacket menu @italic{File>Package Manager},
or run the raco command:
@commandline{raco pkg install quickscript}

You need to restart DrRacket. Now you should have a new item @italic{Scripts} in the menu bar.

@section{Make your own script: First simple example}

Click on the @italic{Scripts>Manage scripts>New script...} menu item, and enter @italic{Reverse} for the script name.
This creates and opens the file reverse.rkt in the user's scripts directory.
Also, a new item automatically appears in the @italic{Scripts} menu.

In the .rkt file that just opened in DrRacket, modify the @racket[define-script] definition to the following:
@(racketblock
  (define-script reverse
    #:label "Reverse"
    (λ(selection)
      (list->string (reverse (string->list selection))))))
and save the file.
(Note: if you later change the @racket[label] property, you will need to reload the menu by clicking on
@italic{Scripts>Manage scripts>Reload scripts menu} after saving the file).

Then go to a new tab, type some text, select it, and click on @italic{Scripts>Reverse}, and voilà!

@section{Into more details}

Quickscript adds a @italic{Scripts} menu to the main DrRacket window.
This menu has several items, followed by the list of scripts.

The @italic{New script} item asks for a script name and creates a corresponding .rkt file
in the user's script directory, and opens it in DrRacket.

Each scripts is defined with @racket[define-script], which among other things adds an entry in DrRacket's Scripts menu.
A single script file can contain several calls to @racket[define-script].

By default, the new script is reduced to its simplest form.
However, scripts can be extended with several optional @italic{properties} and @italic{arguments}.
When all of them are used, a script can look like this:
@#reader scribble/comment-reader
(racketblock
  (define-script a-complete-script
    ;; Properties:
    #:label "Full script"
    #:help-string "A complete script showing all properties and arguments"
    #:menu-path ("Submenu" "Subsubmenu")
    #:shortcut #\a
    #:shortcut-prefix (ctl shift)
    #:output-to selection
    #:persistent
    #:os-types (unix macosx windows)
    ;; Procedure with its arguments:
    (λ(selection #:editor ed #:frame fr #:interactions ints #:file f)
      "Hello world!")))

Below we detail first the procedure and its arguments and then the script's properties.

@subsection{The script's procedure}

When clicking on a script label in the Scripts menu in DrRacket,
its corresponding procedure is called.
The procedure takes at least the @racket[selection] argument, which is the string that is currently
selected in the current editor.
The procedure must returns either @racket[#f] or a @racket[string?].
If it returns @racket[#f], no change is applied to the current editor, but if it returns a string,
then the current selection is replace with the return value.

If some of the above keywords are specified in the procedure, the Script Plugin detects them and passes the
corresponding values, so the procedure can take various forms:
@(racketblock
  (λ(selection)....)
  (λ(selection #:frame fr)....)
  (λ(selection #:file f)....)
  (λ(selection #:editor ed #:file f)....)
  ....
  )

Here is the meaning of the keyword arguments:
@itemlist[
 @item{@racket[#:file : (or/c path? #f)]

  The path to the current file of the definition window, or @racket[#f]
  if there is no such file (i.e., unsaved editor).

  @bold{Example:}
  @(racketblock
    (define-script current-file-example
      #:label "Current file example"
      #:output-to message-box
      (λ(selection #:file f)
        (string-append "File: " (if f (path->string f) "no-file")
                       "\nSelection: " selection))))

  See also: @racket[file-name-from-path], @racket[filename-extension],
  @racket[path->string], @racket[split-path].
 }

 @item{@racket[#:definitions : text%]

  The @racket[text%] editor of the current definition window.
  See @racket[text%] for more details.
 }

 @item{@racket[#:interactions : text%]

  The @racket[text%] editor of the current interaction window.
  Similar to @racket[#:definitions].
 }

 @item{@racket[#:editor : text%]

  The @racket[text%] current editor, either the definition or the interaction editor.
  Similar to @racket[#:definitions].
 }

 @item{@racket[#:frame : drracket:unit:frame<%>]

  DrRacket's frame.
  For advanced scripting.

  @bold{Example:}
  @(racketblock
    (require racket/class)
    (define-script number-tabs
      #:label "Number of tabs"
      #:output-to message-box
      (λ(selection #:frame fr)
        (format "Number of tabs in DrRacket: ~a"
                (send fr get-tab-count)))))
 }]

@subsection{The script's properties}

The properties are mere data and cannot contain expressions.

Most properties (@racket[#:label], @racket[#:shortcut], @racket[#:shortcut-prefix], @racket[#:help-string]) are the same as
for the @racket[menu-item%] constructor.
In particular, a keyboard shortcut can be assigned to an item.

If a property does not appear in the dictionary, it takes its default value.

There are some additional properties:
@itemlist[
 @item{@racket[#:menu-path : (listof string?) = ()]
  This is the list of submenus in which the script's label will be placed,
  under the Script menu.

  Note that different scripts in different files can share the same submenus.

 }
 @item{@racket[#:output-to : (one-of/c selection new-tab message-box clipboard #f) = selection]

  If @racket[selection], the output of the procedure replaces the
  selection in the current editor (definitions or interactions),
  or insert the output at the cursor if there is no selection.
  If @racket[new-tab], the return value is written in a new tab.
  If @racket[message-box], the return value (if a string) is displayed in a @racket[message-box].
  If @racket[clipboard], the return value (if a string) is copied to the clipboard.
  If @racket[#f], the return value is not used.
 }
 @item{@racket[#:persistent]

  If they keyword @racket[#:persistent] is @emph{not} provided,
  each invocation of the script is done in a fresh namespace
  that is discarded when the procedure finishes.

  But if @racket[#:persistent] is provided, a fresh namespace is created only
  the first time it is invoked, and the same namespace is re-used for the subsequent invocations.
  Note that a single namespace is kept per file, so if different scripts in the same file
  are marked as persistent, they will all share the same namespace (and, thus, variables).
  Also note that a script marked as non-persistent will not share the same namespace as
  the other scripts of the same file marked as persistent.

  Consider the following script:
  @(racketblock
    (define count 0)

    (define-script persistent-counter
      #:label "Persistent counter"
      #:persistent
      #:output-to message-box
      (λ(selection)
        (set! count (+ count 1))
        (number->string count))))

  If the script is persistent, the counter increases at each invocation of the script via the menu,
  whereas it always displays 1 if the script is not persistent.

  Note: Persistent scripts can be "unloaded" by clicking on the @italic{Scripts>Manage scripts>Unload persistent scripts} menu item.
  In the previous example, this will reset the counter.

  @;See a more detailed example in @example-link{persistent-counter.rkt}.

  }
 
 @item{@racket[#:os-types (listof (one-of/c unix macosx windows))]
        
  This keyword must be followed by a list of supported os-types.
  Defaults to all types, i.e. @racket[(unix macosx windows)].
  
 }]

If changes are made to these properties, the Scripts menu will probably need to be reloaded
by clicking on @italic{Scripts>Manage scripts>Reload scripts menu}.

@section{Script library}

When the user creates a new script, the latter is placed into a sub-directory of
@racket[(find-system-path 'pref-dir)].
A direct access to this folder is provided via the @italic{Scripts>Manage scripts>Open script...} menu entry.

Additional directories to look for scripts can be added via the @italic{Scripts>Manage scripts>Library} menu entry.
When a directory is added to the library, all its .rkt files (non-recursively) are considered as scripts.
Specific files can be excluded from the library.

@section{Shadow scripts}

todo...

@section{Updating the quickscript package}

To update Quickscript once already installed,
either do so through the @italic{File>Package Manager} menu in DrRacket,
or run @tt{raco pkg update quickscript}.

The user's scripts will not be modified in the process.

@section{License}

MIT License

Copyright (c) 2012 by @link["mailto:laurent.orseauREMOVEME@gmail.com"]{Laurent Orseau @"<laurent.orseauREMOVEME@gmail.com>"}.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
