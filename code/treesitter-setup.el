;;; treesitter-setup.el --- Setup file for treesitter -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup grammars for the treesitter.

;;; Code:
(add-hook 'prog-mode-hook #'electric-pair-mode)
(use-package treesit
  :ensure nil  ; built-in
  :custom
  (treesit-font-lock-level 4))  ; maximum highlighting

(use-package treesit-auto
  :ensure (:host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)  ; or t for automatic
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . treesit-fold-mode)
  :bind (("C-c f t" . treesit-fold-toggle)
         ("C-c f c" . treesit-fold-close-all)
         ("C-c f o" . treesit-fold-open-all)))

(use-package treesit-fold-indicators
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (treesit-fold-mode-hook . treesit-fold-indicators-mode))

;; TODO: make it respect evil-based "za"
(use-package treesit-jump
  :ensure (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :config
  ;; TODO: Filter out cluttered results
  (setq treesit-jump-queries-filter-list '("inner" "test" "param")))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-c l")
  :hook ((python-ts-mode . combobulate-mode)
         (go-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (toml-ts-mode . combobulate-mode)))

(provide 'treesitter-setup)
;;; treesitter-setup.el ends here
