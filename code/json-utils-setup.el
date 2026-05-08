;;; json-utils-setup.el --- Packages to work with json files -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:


(use-package jq-mode
  :ensure (:host github :repo "ljos/jq-mode")
  :mode ("\\.jq\\'" . jq-mode)
  :bind (:map json-ts-mode-map
              ("C-c C-j" . jq-interactively))
  :custom
  (jq-interactive-default-prompt ".")
  (jq-interactive-font-lock-mode 'json-ts-mode))


(use-package json-ts-mode
  :ensure nil  ; built-in
  :mode "\\.json\\'")



(provide 'json-utils-setup)
;;; json-utils-setup.el ends here
