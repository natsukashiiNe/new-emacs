;;; js-ts-settings.el --- Settings for JavaScript / TypeScript. -*- lexical-binding: t; -*-

;;; Commentary:
;; JavaScript / TypeScript / JSX / TSX:
;;   - built-in tree-sitter modes (typescript-ts-mode, tsx-ts-mode, js-ts-mode)
;;   - typescript-language-server via lsp-mode's built-in `ts-ls' client
;;   - ESLint as an add-on LSP, repointed at the system binary
;;   - prettier via apheleia
;;   - combobulate structural editing on the -ts-mode variants
;;   - a C-c h mode-local keymap mirroring python-settings.el
;;
;; ESLint: lsp-mode already ships an `eslint' client that is :add-on? and
;; activates on js / jsx / ts / tsx / html / svelte.  We do NOT register a
;; second client -- we only repoint its server command at the system
;; `vscode-eslint-language-server' (from vscode-langservers-extracted) so
;; it stops trying to download the bundled VSIX.
;;
;; Note: no `indent-bars-treesit-scope' entry for js/ts/tsx.  The indent
;; bars themselves are drawn from indentation and need no tree-sitter; the
;; scope list is only the optional "emphasize the innermost scope" feature.
;; indent-bars ships no built-in scope defaults and the community wiki has
;; none for js/ts, so it would be a hand-maintained list against grammar
;; node names that drift (tree-sitter renamed `function' to
;; `function_expression', which threw a treesit-query-error).  Not worth it
;; -- bars work fine without it.

;;; Code:

;; =============================================================================
;; TREE-SITTER MODES  (built-in -- treesit-auto fetches the grammars)
;; =============================================================================

(use-package typescript-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode)))

(use-package tsx-ts-mode
  :ensure nil  ; built-in (lives in typescript-ts-mode.el)
  :mode "\\.tsx\\'")

(use-package js-ts-mode
  :ensure nil  ; built-in (lives in js.el)
  :mode (("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)))

;; --- Legacy typescript-mode (not a major mode we land in) ---
;; We edit .ts/.tsx in the built-in typescript-ts-mode above; this old
;; package is installed only because OTHER tools embed it as a sub-mode.
;; Today its consumer is svelte-mode, which fontifies `<script lang="ts">'
;; blocks by `(require 'typescript-mode nil t)' -- without it those blocks
;; render unhighlighted.  It claims no file extensions (no auto-mode-alist
;; autoload), so it never steals .ts/.tsx from typescript-ts-mode.
(use-package typescript-mode
  :ensure t
  :defer t)

;; =============================================================================
;; APHELEIA FORMATTER (prettier)
;; =============================================================================
;; apheleia's built-in `prettier' formatter runs via `apheleia-npx', which
;; prefers the project-local ./node_modules/.bin/prettier -- so project
;; plugins resolve -- and passes --stdin-filepath so the parser is picked
;; by extension.

(with-eval-after-load 'apheleia
  (dolist (m '(typescript-ts-mode tsx-ts-mode js-ts-mode))
    (setf (alist-get m apheleia-mode-alist) 'prettier)))

;; =============================================================================
;; LSP  (typescript-language-server + ESLint add-on)
;; =============================================================================

(with-eval-after-load 'lsp-mode
  (setq lsp-typescript-suggest-complete-function-calls t
        lsp-javascript-suggest-complete-function-calls t))

;; Repoint the built-in `eslint' client at the system binary instead of the
;; downloaded VSIX.  `lsp-eslint-server-exists?' resolves a non-"node"
;; command via `executable-find', so this just works once the package is on
;; PATH.  defcustom will not clobber a value already set here.
(setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))

(dolist (h '(typescript-ts-mode-hook
             tsx-ts-mode-hook
             js-ts-mode-hook))
  (add-hook h #'lsp-deferred))

;; =============================================================================
;; COMBOBULATE  (structural editing)
;; =============================================================================
;; combobulate-mode is autoloaded, so hooking directly pulls it in on the
;; first ts/tsx buffer -- mirrors the hook list in treesitter-setup.el.

(add-hook 'svelte-mode-hook #'highlight-indent-guides-mode)

(add-hook 'typescript-ts-mode-hook #'combobulate-mode)
(add-hook 'tsx-ts-mode-hook        #'combobulate-mode)

;; =============================================================================
;; MODE-LOCAL KEYMAP -- C-c h
;; =============================================================================
;; LSP-centric (no REPL for JS/TS).  Mirrors python-settings.el's convention.
;; Shared submap installed on all three mode maps.

(defvar my/js-ts-cc-h-map
  (define-keymap
    ;; refactors / navigation
    "o" #'lsp-organize-imports
    "a" #'lsp-execute-code-action
    "r" #'lsp-rename
    "R" #'lsp-find-references
    ;; docs
    "d" #'lsp-ui-doc-glance
    "D" #'lsp-describe-thing-at-point
    ;; format / lint
    "f" #'apheleia-format-buffer
    "x" #'lsp-eslint-fix-all)
  "Shared C-c h submap for JavaScript / TypeScript modes.")

(with-eval-after-load 'typescript-ts-mode
  (keymap-set typescript-ts-mode-map "C-c h" my/js-ts-cc-h-map)
  (keymap-set tsx-ts-mode-map        "C-c h" my/js-ts-cc-h-map))

(with-eval-after-load 'js
  (keymap-set js-ts-mode-map "C-c h" my/js-ts-cc-h-map))

(provide 'js-ts-settings)
;;; js-ts-settings.el ends here
