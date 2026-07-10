;;; svelte-settings.el --- Settings for Svelte / SvelteKit. -*- lexical-binding: t; -*-

;;; Commentary:
;; Svelte:
;;   - svelte-mode (leafOfTree/svelte-mode) for .svelte
;;   - svelte-language-server via lsp-mode's built-in `svelte-ls' client;
;;     it activates on the .svelte extension and locates `svelteserver' on
;;     PATH, so nothing needs registering here.
;;   - emmet-mode in the markup region
;;   - prettier (+ prettier-plugin-svelte, project-local) via apheleia
;;   - a C-c h mode-local keymap
;;
;; Keeping svelte-mode loadable also lets Org `#+begin_src svelte' blocks
;; fontify -- the original reason svelte-mode was pulled in.
;;
;; Two LSPs attach to a .svelte buffer on purpose: svelte-ls (types /
;; template) and eslint (style, configured in js-ts-settings.el).  The TS
;; server is deliberately NOT attached -- svelte-ls proxies TypeScript for
;; <script lang="ts"> blocks itself.

;;; Code:

;; =============================================================================
;; SVELTE MODE
;; =============================================================================

(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'")

;; --- indent-bars conflicts with svelte-mode -> turn it off here ---
;; svelte-mode (mhtml-style) installs its own `font-lock-fontify-region-
;; function' to fontify <script>/<style>/<template> sub-regions.  indent-bars
;; ALSO wraps that function (`indent-bars--fontify').  Stacked, they re-enter
;; during jit-lock redisplay and blow the Lisp stack:
;;   Error during redisplay: (jit-lock-function N) signaled
;;   (excessive-lisp-nesting ...)
;; This is the same indent-bars font-lock re-entry already worked around for
;; CIDER (see clojure-settings.el).  There is no clean coexistence, so we just
;; disable indent-bars in svelte buffers -- the bars are not worth a broken
;; buffer, and svelte files are rarely deeply nested.  Once it is gone, the
;; <script lang="ts"> submode (typescript-mode, declared in js-ts-settings.el)
;; fontifies normally.
(defun my/svelte-disable-indent-bars ()
  "Turn off `indent-bars-mode' in the current svelte buffer."
  (when (bound-and-true-p indent-bars-mode)
    (indent-bars-mode -1)))
;; Appended so it runs after anything that may have enabled indent-bars
;; during mode setup.
(add-hook 'svelte-mode-hook #'my/svelte-disable-indent-bars 90)

;; =============================================================================
;; APHELEIA FORMATTER (prettier + prettier-plugin-svelte)
;; =============================================================================
;; The svelte plugin must be a project devDependency; apheleia's prettier
;; runs the project-local binary so the plugin resolves.

;; (with-eval-after-load 'apheleia
;;   (setf (alist-get 'svelte-mode apheleia-mode-alist) 'prettier))

(with-eval-after-load 'apheleia
  (setf (alist-get 'prettier-svelte apheleia-formatters)
        '("npx" "prettier" "--parser" "svelte"))
  (setf (alist-get 'svelte-mode apheleia-mode-alist)
        'prettier-svelte))

;; =============================================================================
;; TAME THE CURSOR-DRIVEN SUBMODE SWAP
;; =============================================================================
;; svelte-mode is a multi-mode (mhtml-style): `svelte--pre-command' runs on
;; BOTH pre- and post-command-hook and, when point crosses into/out of a
;; <script>/<style> block, SWAPS a set of "crucial" buffer-locals to that
;; sub-mode's values.  The swapped set includes `major-mode' (=> typescript-
;; mode inside <script lang="ts">, css-mode inside <style>, svelte-mode in
;; markup) and `completion-at-point-functions' (anything matching "completion-").
;; That single mechanism is why `describe-mode' and behaviour change with the
;; cursor -- and it breaks two LSP-driven things:
;;
;;   1. apheleia picks its formatter from `major-mode', so a save formatted the
;;      file differently (or errored) depending on where point sat -- svelte
;;      parser in markup, but ts/css parser over the WHOLE .svelte file inside
;;      a block.  Fix: pin the formatter buffer-locally; apheleia prefers
;;      `apheleia-formatter' over the `major-mode' lookup
;;      (apheleia-formatters.el: `(or apheleia-formatter ...)').
;;
;;   2. the swap overwrites `completion-at-point-functions', dropping the LSP
;;      capf in some regions -> no corfu completion in {template} expressions.
;;      Fix: re-assert the LSP capf after every swap so it is always present.
;;
;; Both are surgical: the swap still runs, so per-region commenting,
;; indentation and the submode mode-line keep working.

(defun my/svelte-pin-formatter ()
  "Always format svelte buffers with the svelte prettier, ignoring the
cursor-swapped `major-mode'."
  (setq-local apheleia-formatter 'prettier-svelte))
(add-hook 'svelte-mode-hook #'my/svelte-pin-formatter)

(defun my/svelte-keep-lsp-capf ()
  "Ensure LSP's capf survives svelte-mode's per-region variable swap."
  (when (bound-and-true-p lsp-mode)
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point)))
(with-eval-after-load 'svelte-mode
  (advice-add 'svelte--pre-command :after #'my/svelte-keep-lsp-capf))

;; =============================================================================
;; LSP + EMMET
;; =============================================================================

(add-hook 'svelte-mode-hook #'lsp-deferred)
;; emmet-mode is autoloaded; hook directly so it works even when no
;; html/css buffer has been opened yet.
(add-hook 'svelte-mode-hook #'emmet-mode)

;; =============================================================================
;; MODE-LOCAL KEYMAP -- C-c h
;; =============================================================================

(with-eval-after-load 'svelte-mode
  (keymap-set svelte-mode-map "C-c h"
              (define-keymap
                ;; refactors / navigation
                "o" #'lsp-organize-imports
                "a" #'lsp-execute-code-action
                "r" #'lsp-rename
                "R" #'lsp-find-references
                ;; docs
                "d" #'lsp-ui-doc-glance
                "D" #'lsp-describe-thing-at-point
                ;; format / emmet
                "f" #'apheleia-format-buffer
                "e" #'emmet-expand-line)))

(provide 'svelte-settings)
;;; svelte-settings.el ends here
