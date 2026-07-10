;;; ego-setup.el --- Setup for my 'ego' package. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(require 'ego)

(setq ego-ql-locations my-local--ego-ql-locations)
(setq ego-ql-global-prefix     "C-c L")
(setq ego-ql-minibuffer-prefix "C-c L")
(setq ego-ql-dired-prefix      "M-f")

(ego-ql-setup)

(keymap-set ego-ql--map "C-d" #'ego-open-dirvish-at-quick-location)

(provide 'ego-setup)
;;; ego-setup.el ends here
