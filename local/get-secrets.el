;;; get-secrets.el --- Retrieve secrets. -*- lexical-binding: t; -*-

;;; Commentary:
;; File to setup secrets retrieval.
;; Usually gets them from the system / gpg.

;;; Code:

(require 'epa-file)
(epa-file-enable)

(setq auth-sources '("~/.authinfo.gpg")) ;; Magit
(setq epg-pinentry-mode 'loopback)       ;; allows GPG passphrase prompts in Emacs

;; TODO: get keys for AI assistants.

(provide 'get-secrets)
;;; get-secrets.el ends here
