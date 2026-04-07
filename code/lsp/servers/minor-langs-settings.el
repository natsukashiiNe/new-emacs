;;; minor-langs-settings.el --- Settings for minor languages and data formats. -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for languages that need minimal or no LSP setup:
;; bash, cmake, csharp, yuck, kdl, json, yaml, toml, dockerfile.

;;; Code:

;; =============================================================================
;; BASH
;; =============================================================================

(use-package bash-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("bashrc\\'" . bash-ts-mode)
         ("bash_profile\\'" . bash-ts-mode)))

(add-hook 'bash-ts-mode-hook #'lsp-deferred)
(add-hook 'sh-mode-hook #'lsp-deferred)

;; =============================================================================
;; CMAKE
;; =============================================================================

(use-package cmake-ts-mode
  :ensure nil  ; built-in
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(add-hook 'cmake-ts-mode-hook #'lsp-deferred)
(add-hook 'cmake-mode-hook #'lsp-deferred)

;; =============================================================================
;; YUCK
;; =============================================================================

(use-package yuck-mode
  :ensure t)

;; =============================================================================
;; KDL
;; =============================================================================

(use-package kdl-mode
  :ensure t
  :mode "\\.kdl\\'"
  :hook (kdl-mode . (lambda ()
                      (when (treesit-available-p)
                        (treesit-parser-create 'kdl))))
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(kdl-mode . "kdl"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "kdl-lsp")
      :activation-fn (lsp-activate-on "kdl")
      :server-id 'kdl-lsp))))

;; =============================================================================
;; DATA FORMATS
;; =============================================================================

(use-package json-ts-mode
  :ensure nil  ; built-in
  :mode "\\.json\\'")

(use-package yaml-ts-mode
  :ensure nil  ; built-in
  :mode "\\.ya?ml\\'")

(use-package toml-ts-mode
  :ensure nil  ; built-in
  :mode "\\.toml\\'")

(use-package dockerfile-ts-mode
  :ensure nil  ; built-in (Emacs 29.1+)
  :mode "Dockerfile\\'")

(provide 'minor-langs-settings)
;;; minor-langs-settings.el ends here
