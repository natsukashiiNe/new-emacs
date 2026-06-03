;;; clojure-settings.el --- Settings for Clojure / Jank. -*- lexical-binding: t; -*-

;;; Commentary:
;; Full Clojure (and jank dialect) integration:
;;   - clojure-ts-mode for .clj / .cljs / .cljc / .edn / .bb / .jank
;;   - clojure-lsp via lsp-mode
;;   - cljfmt via apheleia
;;   - CIDER for the interactive dev loop (REPL, eval, tests, jack-in)
;;   - clojure-snippets for yasnippet
;;   - Paren / sexp visual aids matching the existing lisp-editing UX
;;   - A C-c h mode-local keymap mirroring python-settings.el's convention
;;
;; Jank: shares Clojure surface syntax, so clojure-ts-mode + clojure-lsp +
;; clj-kondo work for editing.  The REPL is reached via
;; `my/jank-cider-connect' against a running `jank --repl --port N'.
;;
;; Caveat: CIDER's default cider-mode-map binds many actions under C-c C-*,
;; which overlaps with our global `my-maps/compile-map' at C-c C.  We leave
;; cider-mode-map intact -- shadowing the global maps is the price of CIDER.

;;; Code:

;; =============================================================================
;; INDENT BARS
;; =============================================================================

(with-eval-after-load 'indent-bars
  (add-to-list 'indent-bars-treesit-scope
               '(clojure list_lit map_lit vec_lit set_lit anon_fn_lit)))

;; =============================================================================
;; TREE-SITTER GRAMMAR PIN
;; =============================================================================
;; clojure-ts-mode targets a specific commit of sogaiu/tree-sitter-clojure
;; (see `clojure-ts-grammar-recipes' in clojure-ts-mode.el).  treesit-auto's
;; default recipe omits the version, so on a fresh install it pulls HEAD,
;; clojure-ts-mode detects the mismatch, and reinstalls the right commit --
;; but the wrong grammar is already loaded into the running Emacs and
;; tree-sitter cannot hot-swap.  Pinning both sides to the same source
;; avoids the race entirely.  `require' treesit eagerly because this file
;; loads before treesitter-setup.el, so the variable does not exist yet.
(require 'treesit)
(add-to-list 'treesit-language-source-alist
             '(clojure "https://github.com/sogaiu/tree-sitter-clojure.git"
                       "unstable-20250526"))

;; =============================================================================
;; TREE-SITTER MODE
;; =============================================================================

(use-package clojure-ts-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-mode)
         ("\\.edn\\'"  . clojure-ts-mode)
         ("\\.bb\\'"   . clojure-ts-mode)
         ("\\.jank\\'" . clojure-ts-mode)))

;; =============================================================================
;; APHELEIA FORMATTER (cljfmt)
;; =============================================================================
;; cljfmt reads stdin / writes stdout.  Install via:
;;   bbin install io.github.weavejester/cljfmt

(with-eval-after-load 'apheleia
  (setf (alist-get 'cljfmt apheleia-formatters)
        '("cljfmt" "fix" "-"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'cljfmt)
  (setf (alist-get 'clojure-ts-mode apheleia-mode-alist) 'cljfmt))

;; =============================================================================
;; LSP (clojure-lsp)
;; =============================================================================

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(clojure-ts-mode . "clojure"))
  (setq lsp-clojure-custom-server-command '("clojure-lsp")))

;; Disable clojure-lsp's on-save formatting so apheleia + cljfmt stay the
;; single source of truth.
(with-eval-after-load 'lsp-clojure
  (setq lsp-clojure-server-arguments
        '("--settings" "{:format {:enabled false}}")))

(add-hook 'clojure-ts-mode-hook #'lsp-deferred)
(add-hook 'clojure-mode-hook    #'lsp-deferred)

;; =============================================================================
;; CIDER (interactive dev / REPL)
;; =============================================================================

(use-package cider
  :ensure t
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  (cider-save-file-on-load t)
  (cider-eldoc-display-for-symbol-at-point t)
  (cider-overlays-use-font-lock t)
  (cider-show-error-buffer 'only-in-repl)
  (cider-prompt-for-symbol nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-history-file
   (expand-file-name "cider-history" "~/.local/emacs/"))
  ;; CIDER installs its own `font-lock-fontify-region-function' on every
  ;; connected clojure buffer to recolor vars known to the REPL.  Stacked
  ;; on top of indent-bars' tree-sitter font-lock wrapper, that closure
  ;; re-enters and never terminates -- Emacs locks at 75% CPU on the first
  ;; `jit-lock-fontify-now' after `cider-connect-cljs'.  Disabling dynamic
  ;; font-lock keeps the wrapper installed but makes it short-circuit.
  (cider-font-lock-dynamically nil)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'insert cider-repl-mode-map
      (kbd "RET") #'newline
      (kbd "C-c RET") #'cider-repl-return)
    )
  )

;; =============================================================================
;; SNIPPETS
;; =============================================================================

(use-package clojure-snippets
  :ensure t
  :after yasnippet)

;; =============================================================================
;; PAREN / SEXP VISUAL AIDS
;; =============================================================================
;; Reuse the same overlays that power the elisp / CL editing experience in
;; code/lisp-editing.el.

(with-eval-after-load 'highlight-sexp
  (add-hook 'clojure-ts-mode-hook #'highlight-sexp-mode)
  (add-hook 'cider-repl-mode-hook #'highlight-sexp-mode))

(with-eval-after-load 'highlight-parentheses
  (add-hook 'clojure-ts-mode-hook #'highlight-parentheses-mode)
  (add-hook 'cider-repl-mode-hook #'highlight-parentheses-mode))

;; Lispyville is currently dormant in lisp-editing.el (loads :after lispy,
;; which is commented out).  If it gets re-enabled, this hook will pick up
;; clojure-ts-mode automatically.
(with-eval-after-load 'lispyville
  (add-hook 'clojure-ts-mode-hook #'lispyville-mode))

;; =============================================================================
;; JANK REPL CONNECT HELPER
;; =============================================================================

(defun my/jank-cider-connect (host port)
  "Connect CIDER to a running jank REPL at HOST:PORT.
Start jank first with `jank --repl --port PORT' in the project root."
  (interactive
   (list (read-string "Jank host: " "localhost")
         (read-number  "Jank port: " 7888)))
  (cider-connect-clj (list :host host
                           :port port
                           :project-dir
                           (when-let* ((p (project-current)))
                             (project-root p)))))

;; =============================================================================
;; MODE-LOCAL KEYMAP -- C-c h
;; =============================================================================
;; mirrors python-settings.el convention
;; potentially will need to wipe out the default cider map of C-c C-*
;; (as with python) - can conflict with my maps. (under global map)

(with-eval-after-load 'clojure-ts-mode
  (keymap-set clojure-ts-mode-map "C-c h"
              (define-keymap
                ;; eval
                "e"   #'cider-eval-last-sexp
                "E"   #'cider-eval-last-sexp-to-repl
                "b"   #'cider-eval-buffer
                "r"   #'cider-eval-region
                "n"   #'cider-eval-ns-form
                "x"   #'cider-eval-defun-at-point
                ;; inspect / docs / macroexpand
                "d"   #'cider-doc
                "D"   #'cider-clojuredocs
                "i"   #'cider-inspect
                "m"   #'cider-macroexpand-1
                "M"   #'cider-macroexpand-all
                ;; tests
                "t"   #'cider-test-run-test
                "T"   #'cider-test-run-ns-tests
                "P"   #'cider-test-run-project-tests
                ;; jack-in / connect
                "j j" #'cider-jack-in-clj
                "j s" #'cider-jack-in-cljs
                "j b" #'cider-jack-in-babashka
                "j c" #'cider-connect-clj
                "j J" #'my/jank-cider-connect
                ;; namespace / repl
                "N"   #'cider-repl-set-ns
                "u"   #'cider-undef
                "z"   #'cider-switch-to-repl-buffer)))

(with-eval-after-load 'which-key
  (with-eval-after-load 'clojure-ts-mode
    (which-key-add-keymap-based-replacements clojure-ts-mode-map
      "C-c h"   "clojure"
      "C-c h j" "jack-in / connect")))

;; =============================================================================
;; CIDER REPL EVIL BINDINGS
;; =============================================================================
;; as in mirror sly / agent-shell conventions

(with-eval-after-load 'cider-repl
  (with-eval-after-load 'evil
    (evil-define-key 'insert cider-repl-mode-map
      (kbd "RET") #'cider-repl-return)
    (evil-define-key '(normal visual) cider-repl-mode-map
      (kbd "C-j") #'cider-repl-next-prompt
      (kbd "C-k") #'cider-repl-previous-prompt
      (kbd "M-p") #'cider-repl-previous-input
      (kbd "M-n") #'cider-repl-next-input)))

;; =============================================================================
;; ORG-BABEL  (#+begin_src clojure ... #+end_src)
;; =============================================================================
;; Two-part setup:
;;
;;  - `org-mode-hook' triggers `(require 'ob-clojure)' the first time any
;;    .org buffer opens.  This is more reliable than relying on
;;    `with-eval-after-load 'org' -- after-load callbacks are wrapped in
;;    `condition-case-unless-debug', so any error inside silently aborts
;;    the callback and the require never completes.  The hook fires in a
;;    real buffer where errors surface normally.
;;
;;  - `with-eval-after-load 'ob-clojure' wires the backend and header args
;;    after the feature actually loads.
;;
;; Jank src blocks reuse the clojure execute function via defalias; CIDER
;; can connect to a running `jank --repl' the same way.

(defun my/org-babel-clojure--ensure-loaded (&rest _)
  "Load ob-clojure (and cider, which provides `cider-buffer-ns' /
`cider-current-connection') and register clojure-ts for source-block
editing.  Idempotent -- safe to call from multiple triggers."
  (require 'ob-clojure)
  ;; ob-clojure only `defvar's cider-buffer-ns without a value -- accessing
  ;; it before CIDER has loaded throws "void variable".  Require the parts
  ;; of CIDER that bind the variable and define the connection helpers.
  (require 'cider)
  (add-to-list 'org-babel-load-languages '(clojure . t))
  ;; Use `setf' / `alist-get' rather than `add-to-list' so that we
  ;; OVERRIDE any pre-existing mapping (e.g. from clojure-mode adding
  ;; ("clojure" . clojure)) instead of just adding a competing entry
  ;; that `assoc' may not return first.
  (setf (alist-get "clojure" org-src-lang-modes nil nil #'equal) 'clojure-ts)
  (setf (alist-get "jank"    org-src-lang-modes nil nil #'equal) 'clojure-ts))

;; Three independent triggers -- any one is enough.  Belt-and-suspenders
;; because `run-hooks' aborts on the first error, so an earlier function in
;; org-mode-hook can prevent ours from running.
(with-eval-after-load 'ob (my/org-babel-clojure--ensure-loaded))
(add-hook 'org-mode-hook #'my/org-babel-clojure--ensure-loaded)
(advice-add 'org-babel-execute-src-block :before
            #'my/org-babel-clojure--ensure-loaded)

;; Set the backend unconditionally and EARLY (before ob-clojure's defcustom
;; auto-detects one based on $PATH).  Otherwise, with /usr/bin/clojure
;; installed, ob-clojure defaults to 'clojure-cli and shells out -- if that
;; shell-out misbehaves the body itself can get returned as the "result",
;; then `org-babel-read' (ob-core.el:3373) does `(eval (read ...))' and
;; trips on the first symbol (range / ->>).
(setq org-babel-clojure-backend 'cider)
(setq org-babel-default-header-args:clojure
      '((:results . "value")
        (:exports . "both")))

(with-eval-after-load 'ob-clojure
  ;; CIDER drives evaluation -- requires an active REPL (cider-jack-in or
  ;; cider-connect-clj).  Swap to 'babashka for instant, REPL-less eval of
  ;; quick examples (does not see project deps).
  (setq org-babel-clojure-sync-nrepl-timeout 20)
  ;; Jank reuses the clojure machinery.
  (defalias 'org-babel-execute:jank #'org-babel-execute:clojure)
  (setq org-babel-default-header-args:jank
        org-babel-default-header-args:clojure))

(provide 'clojure-settings)
;;; clojure-settings.el ends here
