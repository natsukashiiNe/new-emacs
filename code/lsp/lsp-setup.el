;;; lsp-setup.el --- Core LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core lsp-mode configuration with lsp-ui, formatting, and keybindings.
;; Language-specific configurations live under code/lsp/language-settings/.
;; Each language file owns its own apheleia formatter, indent-bars scope,
;; tree-sitter mode, and lsp-mode tunables.

;;; Code:

;; =============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; =============================================================================

;; Increase read process output max for LSP servers (3MB for large UE responses)
(setq read-process-output-max (* 3 1024 1024))

;; Faster garbage collection during LSP operations
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
;; TODO research some more options

;; =============================================================================
;; LSP-MODE - Core Configuration
;; =============================================================================

;; Disable conflicting clients before lsp-mode loads
(with-eval-after-load 'lsp-mode
  (setq lsp-disabled-clients '(alive-lsp ruff-lsp)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ;; Completion
  ;; prevents lsp from force-enabling company (CAPF still works).
  (lsp-completion-provider :none)

  ;; Session management
  ;; nil = no session file. Prevents lsp from accumulating workspace folders
  ;; from past sessions and sending them all to newly started servers.
  ;; Root detection via project.el (test-project.el) makes the session unnecessary.
  (lsp-session-file nil)

  ;; Keybindings
  (lsp-keymap-prefix "C-c j")

  ;; General settings

  (lsp-modeline-diagnostics-enable nil)
  (lsp-auto-configure t)
  (lsp-enable-snippet t)
  (lsp-prefer-flymake t)
  (lsp-idle-delay 0.2)
  (lsp-log-io nil)          ; Disable for performance

  ;; UI enhancements
  (lsp-enable-symbol-highlighting t)
  (lsp-semantic-tokens-apply-modifiers nil)
  (lsp-semantic-tokens-enable nil)  ; Enable semantic tokens for all servers

  ;; Headerline breadcrumb
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-icons-enable t)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))

  ;; docs
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover t)
  ;; Signature popup (function, params, render-documentation, fit logic)
  ;; lives in `lsp-signature-settings'.

  ;; Icons (nerd-icons for terminal compatibility)
  (lsp-icons-provider 'nerd-icons)

  ;; Project detection
  (lsp-auto-guess-root t)
  ;; Kill server when last buffer closes — forces a fresh start with only
  ;; the current project root instead of reusing a server that has accumulated
  ;; every workspace folder from the session history.
  (lsp-keep-workspace-alive nil)

  ;; Xref integration
  (xref-search-program 'ripgrep)

  :config
  ;; Set xref backend to use LSP
  (setq xref-backend-functions '(lsp--xref-backend))

  ;; Enable which-key integration
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t))

  ;; Reject home dir and bare catch-all dirs as project roots. When a file
  ;; is opened outside a proper project and root detection falls back to ~/
  ;; or ~/working-dir, that path gets stored in the session and sent to
  ;; every future server of that type as a workspace folder.
  ;; (defun my--lsp-reject-shallow-roots (root)
  ;;   (let ((bad (mapcar #'expand-file-name
  ;;                      '("~/" "~/working-dir/" "~/working-dir"))))
  ;;     (unless (member root bad) root)))
  ;; (advice-add 'lsp--suggest-project-root
  ;;             :filter-return #'my--lsp-reject-shallow-roots)
  )


;; =============================================================================
;; SYMBOLS-OUTLINE - tree-view of lsp symbols.
;; =============================================================================

(use-package symbols-outline
  :ensure t
  :custom
  (symbols-outline-no-delete-other-window t)
  (symbols-outline-no-other-window t)
  (symbols-outline-use-nerd-icon-in-gui t)
  (symbols-outline-use-nerd-icon-in-tui t)
  (symbols-outline-window-height 10)
  (symbols-outline-window-width 37))


;; =============================================================================
;; LSP-UI - Enhanced UI Components
;; =============================================================================

(use-package eldoc-box
  :ensure t
  ;; :hook (lsp-mode . eldoc-box-hover-at-point-mode)
  :init
  ;; (defun my/eldoc-box-position-below-cursor (width height)
  ;;   "Position eldoc-box below the cursor with spacing."
  ;;   (let* ((point-pos (posn-at-point))
  ;;          (point-x (car (posn-x-y point-pos)))
  ;;          (point-y (cdr (posn-x-y point-pos)))
  ;;          (line-height (frame-char-height))
  ;;          ;; Position 3 lines below cursor to avoid overlap
  ;;          (spacing (* 3 line-height)))
  ;;     (cons point-x (+ point-y spacing))))

  :custom
  (eldoc-box-max-pixel-width 800)
  (eldoc-box-max-pixel-height 600)
  (eldoc-box-clear-with-C-g t)

  ;; (eldoc-box-position-function #'my/eldoc-box-position-below-cursor)

  ;; (setq eldoc-box-frame-parameters
  ;;       '((left-fringe . 8)
  ;;         (right-fringe . 8)))
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)


  ;; Peek (enable for modal definition/reference viewing)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-peek-height 20)

  ;; Imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-auto-refresh t)

  (lsp-ui-doc-enhanced-markdown t)

  :config
  ;; == Doc frame =======================================
  (setq lsp-ui-doc-text-scale-level 2)    ; +2 scale steps above base font

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-alignment 'window)         ; 'frame or 'window
  (setq lsp-ui-doc-side 'right)               ; 'right or 'left
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-doc-max-width 85)
  (setq lsp-ui-doc-max-height 15)

  ;; ==== Child frame settings =============================
  (setf (alist-get 'internal-border-width lsp-ui-doc-frame-parameters) 2)
  (setf (alist-get 'left-fringe lsp-ui-doc-frame-parameters) 2)
  (setf (alist-get 'right-fringe lsp-ui-doc-frame-parameters) 2)

  (set-face-attribute 'lsp-ui-doc-header nil
		      :height 210        ; TODO: make dynamically deducted (lazy-eval)
		      :weight 'bold)


  ;; Keybindings for lsp-ui-peek
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  ;; keymap

  (evil-define-key 'normal lsp-ui-mode-map (kbd "M-d") #'lsp-ui-doc-toggle)
  )

;; =============================================================================
;; APHELEIA - Code Formatting
;; =============================================================================

(use-package apheleia
  :ensure t
  :demand t
  :config
  ;; Enable auto-formatting on save for all supported modes.
  ;; Per-language formatter definitions and mode mappings live in each
  ;; language-settings file under code/lsp/language-settings/.
  (apheleia-global-mode +1))

;; =============================================================================
;; CUSTOM MENU ENTRIES
;; =============================================================================

(defun my/lsp-find-references-export ()
  "Find references and export them via embark to a persistent buffer."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (run-with-idle-timer 0.1 nil #'embark-export))
    (lsp-find-references)))

(with-eval-after-load 'lsp-mode
  (define-key-after lsp-mode-menu
    [find-references-export]
    '(menu-item "Find references (export)" my/lsp-find-references-export
                :active (lsp-feature? "textDocument/references"))
    'lsp-find-references))

;; =============================================================================
;; INTEGRATION WITH EVIL
;; =============================================================================

;; Ensure lsp-mode keybindings work after Evil loads
(with-eval-after-load 'evil
  (add-hook 'lsp-mode-hook
            (lambda ()
	      (when (boundp 'evil-normal-state-map)
                (evil-normalize-keymaps)))))

;; =============================================================================
;; SIGNATURE POPUP
;; =============================================================================

(require 'lsp-signature-settings)

;; =============================================================================
;; LOAD LANGUAGE SERVER CONFIGURATIONS
;; =============================================================================

(require 'code-lang-deps)

(require 'cpp-settings)
(require 'python-settings)
(require 'rust-settings)
(require 'go-settings)
(require 'java-settings)
(require 'lua-settings)
(require 'dotnet-settings)
(require 'clojure-settings)
(require 'fennel-settings)
(require 'js-ts-settings)
(require 'html-css-settings)
(require 'svelte-settings)
(require 'minor-langs-settings)

(provide 'lsp-setup)
;;; lsp-setup.el ends here
