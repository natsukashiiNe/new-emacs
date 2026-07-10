;;; code-lang-deps.el --- Some deps others langs are using. -*- lexical-binding: t; -*-

;;; Commentary:
;; SLIME for slime-py.

;;; Code:
;; Pin to the v2.32 release tag. The MELPA HEAD has "Version: 2.32.git" which
;; parses as a pre-release (2 32 -4), failing slime-py's (slime "2.32") check.
(use-package slime
  :ensure (:host github :repo "slime/slime" :tag "v2.32")
  :config
  ;; SLIME is only used for Python (via slime-py). Remove its lisp-mode hook
  ;; to avoid conflicting with SLY, which handles all other Lisp modes.
  (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook))

;; slime-star uses git submodules that Elpaca does not initialize.
;; Each submodule is installed as a separate package so its .el files
;; land on load-path before slime-star or SLIME's contrib loader runs.

(use-package slime-breakpoints        ; sldb-show-frame-local, sldb-source-eval
  :ensure (:host github :repo "mmontone/slime-breakpoints")
  :demand t)

(use-package quicklisp-systems
  :ensure (:host github :repo "mmontone/quicklisp-systems")
  :demand t)

(use-package quicklisp-apropos
  :ensure (:host github :repo "mmontone/quicklisp-apropos")
  :demand t)

(use-package slime-critic
  :ensure (:host github :repo "mmontone/slime-critic")
  :demand t)

(use-package slime-help                ; also provides slime-info
  :ensure (:host github :repo "mmontone/slime-doc-contribs"
		 :main "slime-help.el")
  :demand t)

(use-package system-browser            ; also provides system-browser-cl
  :ensure (:host github :repo "mmontone/lisp-system-browser"
		 :main "system-browser.el")
  :demand t)

;; https://github.com/mmontone/slime-star
(use-package slime-star
  :ensure (:host github :repo "mmontone/slime-star")
  :after (slime slime-breakpoints quicklisp-systems quicklisp-apropos
		slime-critic slime-help system-browser))

(provide 'code-lang-deps)
;;; code-lang-deps.el ends here
