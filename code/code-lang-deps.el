;;; code-lang-deps.el --- Some deps others langs are using. -*- lexical-binding: t; -*-

;;; Commentary:
;; SLIME for slime-py.

;;; Code:
;; Pin to the v2.32 release tag. The MELPA HEAD has "Version: 2.32.git" which
;; parses as a pre-release (2 32 -4), failing slime-py's (slime "2.32") check.
(use-package slime
  :ensure (:host github :repo "slime/slime" :tag "v2.32")
  :defer t
  :config
  ;; SLIME is only used for Python (via slime-py). Remove its lisp-mode hook
  ;; to avoid conflicting with SLY, which handles all other Lisp modes.
  (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
  (require 'slime-breakpoints)
  (require 'quicklisp-systems)
  (require 'quicklisp-apropos)
  (require 'slime-critic)
  (require 'slime-help)
  (require 'system-browser)
  (require 'slime-star)
  ;; SLIME's completion-at-point does a *synchronous* swank RPC per call.
  (dolist (hook '(slime-mode-hook slime-repl-mode-hook))
    (add-hook hook (lambda ()
                     (setq-local corfu-auto-delay 0.3)
                     (setq-local corfu-auto-prefix 2)))))

;; slime-star uses git submodules that Elpaca does not initialize.
;; Each submodule is installed as a separate package so its .el files
;; land on load-path before slime-star or SLIME's contrib loader runs.

(use-package slime-breakpoints        ; sldb-show-frame-local, sldb-source-eval
  :ensure (:host github :repo "mmontone/slime-breakpoints")
  :defer t)

(use-package quicklisp-systems
  :ensure (:host github :repo "mmontone/quicklisp-systems")
  :defer t)

(use-package quicklisp-apropos
  :ensure (:host github :repo "mmontone/quicklisp-apropos")
  :defer t)

(use-package slime-critic
  :ensure (:host github :repo "mmontone/slime-critic")
  :defer t)

(use-package slime-help                ; also provides slime-info
  :ensure (:host github :repo "mmontone/slime-doc-contribs"
		 :main "slime-help.el")
  :defer t)

(use-package system-browser            ; also provides system-browser-cl
  :ensure (:host github :repo "mmontone/lisp-system-browser"
		 :main "system-browser.el")
  :defer t)

;; https://github.com/mmontone/slime-star
(use-package slime-star
  :ensure (:host github :repo "mmontone/slime-star")
  :defer t
  :after (slime slime-breakpoints quicklisp-systems quicklisp-apropos
		slime-critic slime-help system-browser))

(provide 'code-lang-deps)
;;; code-lang-deps.el ends here
