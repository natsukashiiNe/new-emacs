;;; lsp-setup.el --- Core LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core lsp-mode configuration with lsp-ui, formatting, and keybindings.
;; Language-specific configurations are in lsp-servers.el

;;; Code:

;; =============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; =============================================================================

;; Increase read process output max for LSP servers
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Faster garbage collection during LSP operations
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
;; TODO research some more options

;; =============================================================================
;; LSP-MODE - Core Configuration
;; =============================================================================

;; Disable conflicting clients before lsp-mode loads
(with-eval-after-load 'lsp-mode
  (setq lsp-disabled-clients '(alive-lsp)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ;; Completion
  (lsp-completion-provider :capf)  ; Integrate with Corfu

  ;; Session management
  (lsp-session-file (expand-file-name "lsp-session-v1" "~/.local/emacs/"))

  ;; Keybindings
  (lsp-keymap-prefix "C-c j")

  ;; General settings
  (lsp-auto-configure t)
  (lsp-enable-snippet nil)  ; Snippets handled by yasnippet
  (lsp-prefer-flymake nil)  ; Use flycheck instead
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
  (lsp-signature-render-documentation t)

  ;; Icons (nerd-icons for terminal compatibility)
  (lsp-icons-provider 'nerd-icons)

  ;; Project detection
  (lsp-auto-guess-root t)

  ;; Xref integration
  (xref-search-program 'ripgrep)

  :config
  ;; Set xref backend to use LSP
  (setq xref-backend-functions '(lsp--xref-backend))

  ;; Enable which-key integration
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t)))

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
  ;; Sideline (disable - using flycheck-inline instead)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)

  ;; Doc frame (enable for better hover documentation)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'at-point)
  ;; (lsp-ui-doc-alignment 'frame)       ; 'frame or 'window
  (lsp-ui-doc-side 'right)               ; 'right or 'left
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 30)

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
  ;; Enable auto-formatting on save for all supported modes
  (apheleia-global-mode +1)

  ;; Configure formatters
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath))

  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format"
          "-assume-filename" filepath
          "-style=file"))  ; Uses .clang-format file discovery

  ;; Add formatter mappings for major modes
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)

  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt)

  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)

  (setf (alist-get 'jq apheleia-formatters)
	'("jq" "."))
  (add-to-list 'apheleia-mode-alist '(json-mode . jq))
  (add-to-list 'apheleia-mode-alist '(json-ts-mode . jq)))

;; =============================================================================
;; KEYBINDINGS
;; =============================================================================

(with-eval-after-load 'lsp-mode
  ;; Evil mode keybindings (normal state)
  (when (featurep 'evil)
    (evil-define-key 'normal lsp-mode-map
      "gd" 'lsp-find-definition
      "gr" 'lsp-find-references
      "gi" 'lsp-find-implementation
      "K"  'lsp-describe-thing-at-point
      "gD" 'lsp-find-declaration))

  ;; Global keybindings under C-c l prefix
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l i") 'lsp-organize-imports)
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c l s") 'lsp-ivy-workspace-symbol)
  (define-key lsp-mode-map (kbd "C-c l R") 'lsp-workspace-restart)
  (define-key lsp-mode-map (kbd "C-c l Q") 'lsp-workspace-shutdown)

  ;; Additional useful bindings
  (define-key lsp-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "M-?") 'lsp-find-references))

;; =============================================================================
;; INTEGRATION WITH EVIL
;; =============================================================================

;; Ensure lsp-mode keybindings work after Evil loads
(with-eval-after-load 'evil
  (add-hook 'lsp-mode-hook
            (lambda ()
              (when (boundp 'evil-normal-state-map)
                (evil-normalize-keymaps)))))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
