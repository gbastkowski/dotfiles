;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom!
        :completion (corfu +icons +orderless +dabbrev)
                    (vertico +icons +childframe)
        :ui         deft                              ; notational velocity for Emacs
                    doom
                    doom-dashboard
                    doom-quit
                    (emoji +unicode +github)
                    hl-todo                           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
                    ;;hydra
                    indent-guides                     ; highlighted indent columns
                    ligatures
                    minimap
                    modeline
                    nav-flash
                    neotree
                    ophints
                    (popup +defaults)                 ; tame sudden yet inevitable temporary windows
                    ;;tabs                            ; a tab bar for Emacs
                    (treemacs +lsp)
                    unicode
                    (vc-gutter +pretty)
                    vi-tilde-fringe
                    window-select
                    workspaces
                    zen
        :editor     (evil +everywhere)
                    file-templates
                    fold
                    ;;(format +onsave)                ; automated prettiness
                    ;;lispy                           ; vim for lisp, for people who don't like vim
                    multiple-cursors                  ; editing in many places at once
                    ;;objed                           ; text object editing for the innocent
                    ;;parinfer                        ; turn lisp into python, sort of
                    ;;rotate-text                     ; cycle region at point between text candidates
                    snippets
                    word-wrap
        :emacs      (dired +icons)
                    electric
                    (ibuffer +icons)
                    undo
                    vc
        :term       eshell                            ; the elisp shell that works everywhere
                    ;;shell                           ; simple shell REPL for Emacs
                    ;;term                            ; basic terminal emulator for Emacs
                    vterm
        :checkers   (syntax +childframe)
                    (spell +aspell +flyspell +everywhere)
                    grammar
        :tools      ;;ai
                    ;;ansible
                    claude-code
                    codex
                    ;;biblio                          ; Writes a PhD for you (citation needed)
                    ;;collab                          ; buffers with friends
                    ;;debugger                        ; FIXME stepping through code, to help you add bugs
                    direnv
                    docker
                    editorconfig
                    ;;ein                             ; tame Jupyter notebooks with emacs
                    (eval +overlay)
                    (lookup +dictionary)
                    lsp
                    (magit +forge)
                    make                              ; run make tasks from Emacs
                    (pass +auth)
                    pdf                               ; pdf enhancements
                    ;;prodigy                         ; FIXME managing external services & code builders
                    ;;rgb                             ; creating color strings
                    ;;taskrunner                      ; taskrunner for all your projects
                    ;;terraform
                    tmux                              ; an API for interacting with tmux
                    tree-sitter
                    ;;upload                          ; map local to remote projects via ssh/ftp
        :os         (:if (featurep :system 'macos) macos)
                    tty                               ; improve the terminal Emacs experience
        :lang       ;;agda                            ; types of types of types of types...
                    ;;beancount                       ; mind the GAAP
                    (cc +lsp)                         ; C > C++ == 1
                    ;;clojure                         ; java with a lisp
                    ;;common-lisp                     ; if you've seen one lisp, you've seen them all
                    ;;coq                             ; proofs-as-programs
                    ;;crystal                         ; ruby at the speed of c
                    ;;csharp                          ; unity, .NET, and mono shenanigans
                    data
                    ;;(dart +flutter)                 ; paint ui and not much else
                    ;;dhall
                    ;;elixir                          ; erlang done right
                    ;;elm                             ; care for a cup of TEA?
                    emacs-lisp
                    ;;erlang                          ; an elegant language for a more civilized age
                    ;;ess                             ; emacs speaks statistics
                    ;;factor
                    ;;faust                           ; dsp, but you get to keep your soul
                    ;;fortran                         ; in FORTRAN, GOD is REAL (unless declared INTEGER)
                    ;;fsharp                          ; ML stands for Microsoft's Language
                    ;;fstar                           ; (dependent) types and (monadic) effects and Z3
                    ;;gdscript                        ; the language you waited for
                    ;;(go +lsp)                         ; the hipster dialect
                    (graphql +lsp)                    ; Give queries a REST
                    ,@(when (string-prefix-p "akiko" (system-name) t)
                        '((haskell +lsp)))            ; a language that's lazier than I am (akiko only)
                    ;;hy                              ; readability of scheme w/ speed of python
                    ;;idris                           ; a language you can depend on
                    json                              ; At least it ain't XML
                    (java +lsp)                       ; the poster child for carpal tunnel syndrome
                    (javascript +lsp +tree-sitter)    ; all(hope(abandon(ye(who(enter(here))))))
                    ;;julia                           ; a better, faster MATLAB
                    (kotlin +lsp)                     ; a better, slicker Java(Script)
                    (latex +fold +lsp)                ; writing papers in Emacs has never been so fun
                    ;;lean                            ; for folks with too much to prove
                    ;;ledger                          ; be audit you can be
                    lua                               ; one-based indices? one-based indices
                    markdown                          ; writing docs for people to ignore
                    ;;nim                             ; python + lisp at the speed of c
                    ;;nix                             ; I hereby declare "nix geht mehr!"
                    ;;ocaml                           ; an objective camel
                    (org +brain +crypt +dragndrop +journal +pandoc +passwords +pomodoro +present +pretty +roam2)
                    ;;php                             ; perl's insecure younger brother
                    plantuml
                    ;;purescript                      ; javascript, but functional
                    python
                    qt                                ; the 'cutest' gui framework ever
                    ;;racket                          ; a DSL for DSLs
                    ;;raku                            ; the artist formerly known as perl6
                    rest
                    ;;rst                             ; ReST in peace
                    ;;(ruby +rails)
                    (rust +lsp)                       ; rust support
                    (scala +lsp +tree-sitter)
                    ;;(scheme +guile)
                    sh
                    ;;sml
                    ;;solidity                        ; do you need a blockchain? No.
                    ;;swift                           ; who asked for emoji variables?
                    ;;terra                           ; Earth and Moon in alignment for performance.
                    (web +lsp +tree-sitter)
                    yaml
                    ;;zig                             ; C, but simpler
       :email       ,@(when (string-prefix-p "akiko" (system-name) t)
                        '((mu4e +org +gmail +offlineimap))) ; email support (akiko only)
                    ;;notmuch
                    ;;(wanderlust +gmail)
       :app         calendar
                    ;;emms
                    ;;everywhere                      ; *leave* Emacs!? You must be joking
                    ;;irc                             ; how neckbeards socialize
                    ;;(rss +org)                      ; emacs as an RSS reader
                    ;;twitter                         ; twitter client https://twitter.com/vnought
       :config      literate
                    (default +bindings +gnupg +smartparens))
