;;; docs-setup.el --- Settings for various docs -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings for various docs.

;;; Code:
(use-package devdocs
  :ensure t
  :bind (("C-h D" . devdocs-lookup)))

(use-package counsel-dash
  :ensure t
  :config
  (setq counsel-dash-docsets-path "~/.local/share/Zeal/Zeal/docsets")
  (setq counsel-dash-common-docsets '("Boost" "C++")))


(provide 'docs-setup)
;;; docs-setup.el ends here
