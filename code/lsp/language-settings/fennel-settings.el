;;; fennel-settings.el --- Settings for Fennel. -*- lexical-binding: t; -*-

;;; Commentary:
;; Fennel (a Lisp compiling to Lua) integration, structured after
;; clojure-settings.el and inheriting the lisp-editing.el visual layer:
;;   - fennel-mode for .fnl  (derives from prog-mode, requires lisp-mode)
;;   - fennel-proto-repl for the interactive dev loop (eval, docs, reload)
;;   - fennel-ls via lsp-mode (registered manually -- no built-in client)
;;   - fnlfmt via apheleia
;;   - Paren / sexp visual aids shared with the elisp / CL / Clojure UX
;;   - A C-c h mode-local keymap mirroring clojure-settings.el
;;
;; Like the Clojure setup (CIDER + clojure-lsp side by side), the REPL and
;; the LSP coexist: fennel-proto-repl drives live evaluation, fennel-ls
;; provides static diagnostics and navigation.

;;; Code:

;; =============================================================================
;; MODE
;; =============================================================================
;; fennel-mode derives from prog-mode and is not tree-sitter based, so -- as
;; with the elisp / CL modes in lisp-editing.el -- there is no
;; indent-bars-treesit-scope entry to add.

(use-package fennel-mode
  :ensure t
  :mode "\\.fnl\\'"
  :config
  ;; Robust, echo-area-driven REPL: separates IO from results and gives
  ;; jump-to-error from stack traces.  Supersedes the bare `fennel-repl'.
  (add-hook 'fennel-mode-hook #'fennel-proto-repl-minor-mode))

;; =============================================================================
;; APHELEIA FORMATTER (fnlfmt)
;; =============================================================================
;; fnlfmt reads stdin / writes stdout with `-'.  Install via:
;;   luarocks install fnlfmt   (or build from technomancy/fnlfmt)

(with-eval-after-load 'apheleia
  (setf (alist-get 'fnlfmt apheleia-formatters) '("fnlfmt" "-"))
  (setf (alist-get 'fennel-mode apheleia-mode-alist) 'fnlfmt))

;; =============================================================================
;; LSP (fennel-ls)
;; =============================================================================
;; lsp-mode ships no Fennel client, so register one the way minor-langs'
;; kdl-mode block does.  Build + install the server:
;;   git clone https://git.sr.ht/~xerool/fennel-ls && cd fennel-ls
;;   make && sudo make install        (-> /usr/local/bin/fennel-ls)
;;   # or, no sudo:  make install PREFIX="$HOME/.local"

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(fennel-mode . "fennel"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "fennel-ls")
    :activation-fn (lsp-activate-on "fennel")
    :server-id 'fennel-ls)))

(add-hook 'fennel-mode-hook #'lsp-deferred)

;; =============================================================================
;; PAREN / SEXP VISUAL AIDS  (shared with lisp-editing.el)
;; =============================================================================
;; fennel-mode derives from prog-mode, so lisp-mode-hook does not fire for it
;; -- attach the overlays explicitly.

(with-eval-after-load 'highlight-sexp
  (add-hook 'fennel-mode-hook #'highlight-sexp-mode))
(with-eval-after-load 'highlight-parentheses
  (add-hook 'fennel-mode-hook #'highlight-parentheses-mode))
;; Picks up automatically if lispyville is ever re-enabled in lisp-editing.el.
(with-eval-after-load 'lispyville
  (add-hook 'fennel-mode-hook #'lispyville-mode))

;; =============================================================================
;; EVIL-AWARE "EVAL LAST SEXP"
;; =============================================================================
;; In evil NORMAL/VISUAL state the block cursor sits *on* a character, so
;; `point' is to its left.  With `(+ 1 2)|' the cursor covers the `)' but
;; point is between `2' and `)', so `eval-last-sexp' reads backwards and
;; evaluates `2' instead of the whole form.  (Insert state puts the caret
;; *after* the `)', which is why it works there.)  This is generic to evil +
;; any eval-last-sexp; defun-style eval (the `x' key) is immune because it
;; evaluates the whole top-level form.  Step one char right in non-insert
;; states so the sexp the cursor visually sits on is the one evaluated.

(defun my/fennel-eval-last-sexp ()
  "Evaluate the sexp before point via `fennel-proto-repl-eval-last-sexp'.
In evil normal/visual state, advance past the char under the block cursor
first so the visually-selected sexp is evaluated."
  (interactive)
  (if (and (bound-and-true-p evil-local-mode)
           (not (evil-insert-state-p))
           (not (eolp)))
      (save-excursion
        (forward-char)
        (call-interactively #'fennel-proto-repl-eval-last-sexp))
    (call-interactively #'fennel-proto-repl-eval-last-sexp)))

;; =============================================================================
;; MODE-LOCAL KEYMAP -- C-c h   (mirrors clojure-settings.el)
;; =============================================================================

(with-eval-after-load 'fennel-proto-repl
  (keymap-set fennel-proto-repl-minor-mode-map "C-x C-e" #'my/fennel-eval-last-sexp))

(with-eval-after-load 'fennel-mode
  (keymap-set fennel-mode-map "C-c h"
              (define-keymap
                ;; eval
                "e" #'my/fennel-eval-last-sexp
                "r" #'fennel-proto-repl-eval-region
                "x" #'fennel-proto-repl-eval-defun
                "b" #'fennel-proto-repl-eval-buffer
                ;; inspect / docs / compile
                "d" #'fennel-proto-repl-show-documentation
                "a" #'fennel-proto-repl-show-arglist
                "m" #'fennel-proto-repl-macroexpand
                "v" #'fennel-view-compilation       ; show generated Lua
                ;; navigation (fennel-mode uses the standard xref backend)
                "." #'xref-find-definitions
                "," #'xref-go-back
                ;; reload / repl
                "R" #'fennel-proto-repl-reload
                "j" #'fennel-proto-repl
                "z" #'fennel-proto-repl-switch-to-repl)))

(with-eval-after-load 'which-key
  (with-eval-after-load 'fennel-mode
    (which-key-add-keymap-based-replacements fennel-mode-map
      "C-c h" "fennel")))

;; =============================================================================
;; REPL EVIL BINDINGS  (mirrors sly / CIDER conventions)
;; =============================================================================
;; fennel-proto-repl-mode derives from comint-mode, but evil-collection has
;; no module for it, so the buffer would otherwise open in NORMAL state where
;; evil shadows RET (comint-send-input) with `evil-ret' and Enter does
;; nothing.  Start it in INSERT state -- exactly what evil-collection does for
;; cider-repl-mode -- and bind RET -> send in both states so Enter always
;; submits regardless of the current evil state.

(with-eval-after-load 'fennel-proto-repl
  (with-eval-after-load 'evil
    (evil-set-initial-state 'fennel-proto-repl-mode 'insert)
    (evil-define-key '(insert normal) fennel-proto-repl-mode-map
      (kbd "RET") #'comint-send-input)
    (evil-define-key '(normal visual) fennel-proto-repl-mode-map
      (kbd "C-j") #'comint-next-prompt
      (kbd "C-k") #'comint-previous-prompt
      (kbd "M-p") #'comint-previous-input
      (kbd "M-n") #'comint-next-input)))

(provide 'fennel-settings)
;;; fennel-settings.el ends here
