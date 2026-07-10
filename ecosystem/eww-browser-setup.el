;;; eww-browser-setup.el --- Short description -*- lexical-binding: t; -*-

;;; Commentary:
;;


;;; Code:
(use-package shr
  :ensure nil)

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package shrface
  :ensure t
  :after shr
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t)  ; different faces per link type
  ;; enable the "..." ellipsis on hidden headlines like org
  (add-hook 'outline-view-change-hook #'shrface-outline-visibility-changed))

(use-package eww
  :ensure nil
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface)
  ;; this sets up imenu so consult-imenu works with page headings
  (defun my/shrface-eww-setup ()
    (unless shrface-toggle-bullets
      (shrface-regexp)
      (setq-local imenu-create-index-function #'shrface-imenu-get-tree)))
  (add-hook 'eww-after-render-hook #'my/shrface-eww-setup))


(use-package hnreader
  :ensure t)

(provide 'eww-browser-setup)
;;; eww-browser-setup.el ends here
