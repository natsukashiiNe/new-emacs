;;; ego-setup.el --- Setup for my 'ego' package. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(require 'ego)

(setq ego-ql-locations my-local--ego-ql-locations)
(setq ego-ql-global-prefix     "C-c G")
(setq ego-ql-dired-prefix      "M-f")
(setq ego-ql-minibuffer-prefix "C-c l")

(ego-ql-setup)

(provide 'ego-setup)
;;; ego-setup.el ends here
