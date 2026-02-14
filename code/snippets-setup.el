;;; snippets-setup.el --- Snippet setup file. -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings for snippets for various modes.

;;; Code:

(use-package yasnippet
  :ensure t
  :demand t
  :init

  (setq yas-snippet-dirs
        (list (expand-file-name "code/snippets" my-config-dir)))
  :config
  (yas-global-mode 1)
  ;; Optional: reduce verbosity
  (setq yas-verbosity 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  ;; Add yasnippet-snippets to the snippets dir
  ;; This keeps the default snippets available alongside the custom ones
  (add-to-list 'yas-snippet-dirs 
               (expand-file-name "yasnippet-snippets" 
                                 (file-name-directory (locate-library "yasnippet-snippets")))))

;; (with-eval-after-load '(yasnippet yasnippet-snippets consult)
;;   (keymap-set yas-minor-mode-map "C-c f F" 'yas-insert-snippet))

(provide 'snippets-setup)
;;; snippets-setup.el ends here
