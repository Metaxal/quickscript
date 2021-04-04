#lang scribble/manual

@(require racket/file
          racket/list
          racket/path
          racket/runtime-path
          scribble/racket
          (for-label quickscript)
          (for-syntax racket/base) ; for build-path in runtime-path
          (for-label racket/gui)
          (for-label drracket/tool-lib))

@(define (codeblock/file file)
   (list
    @(filebox (path->string (file-name-from-path file)) "")
    @(codeblock (file->string file))))

@(define gui italic) @; Who exports @gui{ normally?

@title{Quickscript, a scripting plugin for DrRacket}

@;author{Laurent Orseau}
@(smaller (author+email "Laurent Orseau" "laurent.orseau@gmail.com"))

@section{Introduction}

Quickscript's makes it easy to extend DrRacket with small Racket scripts
to automate some actions in the editor, while avoiding the need to restart DrRacket.

Creating a new script is as easy as a click on @gui{Scripts | New script…}.
Each script is automatically added as an item to the @gui{Scripts} menu, without needing to restart DrRacket.
A keyboard shortcut can be assigned to a script (via the menu item).
By default, a script takes as input the currently selected text, and outputs the replacement text.
There is also direct access to some elements of DrRacket GUI for advanced scripting,
like DrRacket's frame and the definition or interaction editor.

@section{Installation}

Quickscript is installed automatically with DrRacket, so you don't need to do anything.

@subsection{Installing scripts: Quickscript Extra}

You can use Quickscript on its own, but the
@hyperlink["https://pkgs.racket-lang.org/package/quickscript-extra"]{Quickscript Extra}
package has a wide range of useful scripts as well as some 
example scripts intended for customisation by the user.

To install it, either look for @tt{quickscript-extra} in the DrRacket menu @gui{File|Package Manager},
or run the raco command:
@commandline{raco pkg install quickscript-extra}

Then click on @gui{Scripts|Manage scripts|Compile scripts and reload}. (There is no need to restart DrRacket.)

@subsection{Installing scripts: More scripts}

More scripts can be found on the
@hyperlink["https://github.com/racket/racket/wiki/Quickscript-Scripts-for-DrRacket"]{Racket wiki}---you
can add your own scripts there too if you think they may be useful to others.

@section{Make your own script: First simple example}

Click on the @gui{Scripts|Manage scripts|New script...} menu item, and enter @gui{Reverse} for the script name.
This creates and opens the file reverse.rkt in the user's scripts directory.
Also, a new item automatically appears in the @gui{Scripts} menu.

In the .rkt file that just opened in DrRacket, modify the @racket[define-script] definition to the following:
@margin-note{Don't name your script function @racket[reverse], it would shadow Racket's own and make the script hang.}
@margin-note{If you later change the @racket[#:label] property, you will need to reload the menu by clicking on
@gui{Scripts|Manage scripts|Reload scripts menu} after saving the file).}
@(racketblock
  (define-script reverse-selection
    #:label "Reverse"
    (λ (selection) 
      (list->string (reverse (string->list selection))))))
and save the file.

Then go to a new tab, type some text, select it, and click on @gui{Scripts|Reverse}, and voilà!

@section{Into more details}

Quickscript adds a @gui{Scripts} menu to the main DrRacket window.
This menu has several items, followed by the list of scripts.

The @gui{New script} item asks for a script name and creates a corresponding .rkt file
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
    (λ (selection #:frame fr
                  #:editor ed
                  #:definitions defs
                  #:interactions ints
                  #:file f) 
      "Hello world!")))

Note that the arguments of the properties are literals, not expressions, so they must @italic{not} be quoted.
Below we detail first the procedure and its arguments and then the script's properties.

@(define (elem-symbol sym)
   (elem (symbol->string sym) #:style symbol-color))

@(define (grammar-choice . syms)
   (apply
    elem
    (add-between (map (λ (s) (if (symbol? s)
                                 (elem-symbol s)
                                 s))
                      syms)
                 (elem " | "))))
@;subsection{At a glance}

@defmodule[quickscript]
@defform[(define-script name
           property ...
           proc)
         #:grammar
          ; Do not split these line (verbatim typesetting)
         [(property (code:line #:label label-string)
                    (code:line #:help-string string)
                    (code:line #:menu-path (label-string ...))
                    (code:line #:shortcut char #,(elem "|") symbol #,(elem "|") #f)
                    (code:line #:shortcut-prefix (shortcut-prefix ...))
                    (code:line #:persistent? #t #,(elem "|") #f)
                    (code:line #:output-to output-to)
                    (code:line #:os-types (os-type ...) ))
          (shortcut-prefix #,(grammar-choice 'alt 'cmd 'meta 'ctl 'shift 'option))
          (output-to #,(grammar-choice 'selection 'new-tab 'message-box
                                       'clipboard (racket #f)))
          (os-type #,(grammar-choice 'macosx 'unix 'windows))
          (proc (code:line (#,(elem-symbol 'λ) (selection-id
                               [#:editor editor-id]
                               [#:definitions definitions-id]
                               [#:interactions interactions-id]
                               [#:frame frame-id]
                               [#:file file-id])
                             body-expr ...
                             return-expr))) ;string-expr #,(elem "|") void-expr #,(elem "|") #f
          ]
         ]{See the following subsections for a complete description.}

Observe again that the arguments of the properties are literals and not expressions.
This is because the script file is read twice for different purposes.
The first time, Quickscript reads the script file to extract the minimum information necessary to build the menu items in DrRacket.
No Racket operation is performed at this stage so as to be as light and quick as possible.
Then, when the corresponding menu item is clicked, Quickscript reads the script file a second time, this time to actually read and visit the Racket module and call the corresponding procedure.
That is, the script modules are instantiated only on demand to reduce the loading time and memory footprint.

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
  (λ (selection) ....)
  (λ (selection #:frame fr) ....)
  (λ (selection #:file f) ....)
  (λ (selection #:editor ed #:file f) ....)
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
      (λ (selection #:file f) 
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
      (λ (selection #:frame fr) 
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
 @item{@racket[#:output-to : (or/c selection new-tab message-box clipboard #f) = selection]

  If @racket[selection], the output of the procedure replaces the
  selection in the current editor (definitions or interactions),
  or insert the output at the cursor if there is no selection.
  If @racket[new-tab], the return value is written in a new tab.
  If @racket[message-box], the return value (if a string) is displayed in a @racket[message-box].
  If @racket[clipboard], the return value (if a string) is copied to the clipboard.
  If @racket[#f], the return value is not used.

  If this value is changed, make sure to reload the menu with
  @gui{Scripts | Manage scripts | Reload menu}.
 }
 @item{@racket[#:persistent]

  If they keyword @racket[#:persistent] is @emph{not} provided,
  each invocation of the script is done in a fresh namespace.

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
      (λ (selection) 
        (set! count (+ count 1))
        (number->string count))))

  If the script is persistent, the counter increases at each invocation of the script via the menu,
  whereas it always displays 1 if the script is not persistent.

  @bold{Note:} Persistent scripts can be "unloaded" by clicking on the
  @gui{Scripts|Manage scripts|Unload persistent scripts} menu item.
  In the previous example, this will reset the counter. Make sure to unload a persistent script
  after editing it.
  @gui{Scripts|Manage scripts|Reload menu} and @gui{Scripts|Manage scripts|Compile scripts and reload}
  also unload persistent scripts.

  @bold{Technical point:} The script's procedure is called @emph{outside} of the namespace that was
  used to @racket[dynamic-require] it, and inside DrRacket frame's namespace so as to have access
  to objects in this frame.

  @;See a more detailed example in @example-link{persistent-counter.rkt}.

  }
 
 @item{@racket[#:os-types (listof (one-of/c unix macosx windows))]
        
  This keyword must be followed by a list of supported os-types.
  Defaults to all types, i.e. @racket[(unix macosx windows)].
  
 }]

If changes are made to these properties, the Scripts menu will probably need to be reloaded
by clicking on @gui{Scripts|Manage scripts|Reload scripts menu}.

@section{Script library}

When the user creates a new script, the latter is placed into a sub-directory of
@racket[(find-system-path 'pref-dir)].
A direct access to this folder is provided via the @gui{Scripts|Manage scripts|Open script...} menu entry.

Additional directories to look for scripts can be added via the @gui{Scripts|Manage scripts|Library} menu entry.
When a directory is added to the library, all its .rkt files (non-recursively) are considered as scripts.
Specific files can be excluded from the library.

@section{Shadow scripts}

When a script is installed from a third party package (like quickscript-extra), it comes with its set of own values for its properties.
These values may not suit the user who may want to redefine some of them, like the menu path or the keyboard shortcuts.
An obvious choice for the user is to copy/paste the entire script, but this would prevent from benefiting from further bug fixes and enhancements made by the writer of the original script.

To solve this problem, the user can instead make a @italic{shadow script},
which creates a new script in the user's directory, with its own set of properties
that can be changed by the user, but the procedure of this script is bound to that of the original script.

To make a shadow script, open the script library in @gui{Scripts|Manage scripts|Library}, navigate to the third-party script and click on @gui{Shadow}.


@section{Updating the quickscript package}

To update Quickscript once already installed,
either do so through the @gui{File|Package Manager} menu in DrRacket,
or run @tt{raco pkg update quickscript}.

The user's scripts will not be modified in the process.

@section{Distributing your own scripts}

The @emph{simplest} way to distribute a small script s to publish it as a
@hyperlink["https://gist.github.com/"]{gist}
or on @hyperlink["http://pasterack.org/"]{PasteRack}, and share the link.
A user can then copy/paste the contents into a new script.
Don't forget to include a permissive license such as MIT/Apache 2.


The @emph{best} way to distribute scripts is by creating a package---the user only has to install
the package.
Assuming your scripts are stored in the @racket["scripts"] subdirectory,
include a file (say @racket["register.rkt"]) at the root directory of
the package containing the following code:
@margin-note{If the file @racket["register.rkt"] is not at the root,
                         the runtime-path needs to be modified accordingly.}
@codeblock|{
#lang racket/base
(require (for-syntax racket/base
                     racket/runtime-path
                     (only-in quickscript/library
                              add-third-party-script-directory!)))

;; This file is going to be called during setup and will automatically
;; register the scripts subdirectory in quickscript's library.
(begin-for-syntax
  (define-runtime-path script-dir "scripts")
  (add-third-party-script-directory! script-dir))
  }|

You can see an example with
@hyperlink["https://github.com/Metaxal/quickscript-extra"]{quickscript-extra}.

Don't forget to register your package on the
@hyperlink["https://pkgs.racket-lang.org/"]{Racket server}.


@section{License}

MIT License

Copyright (c) 2012-2018 by @link["mailto:laurent.orseau@gmail.com"]{Laurent Orseau @"<laurent.orseau@gmail.com>"}.

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
