;;; lisp-editing.el --- Settings for LISP editing. -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up packages for Elisp and LISP editing.

;;; Code:
(defmacro l (&rest body)
  `(lambda ()
     ,@body))

(defmacro li (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(require 'mode-local)
(setq-mode-local emacs-lisp-mode global-hl-line-mode nil)
(setq-mode-local lisp-mode global-hl-line-mode nil)

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

(keymap-set emacs-lisp-mode-map "C-x E" 'eval-defun)

(use-package eros
  :ensure t
  :defer t
  :config
  (setq eros-mode 1))

;; TODO: make it use numbered arg to move N sexp forwards.
(defun my/evil-insert-at-end-of-sexp ()
  "Jump to end of current sexp and enter insert mode."
  (interactive)
  (puni-end-of-sexp)
  (forward-char 1)  ; move past the closing paren
  (newline)
  (evil-insert-state))

(defun my/evil-insert-before-end-of-sexp ()
  "Jump to end of current sexp and enter insert mode."
  (interactive)
  (puni-end-of-sexp)
  (newline)
  (evil-insert-state))

(use-package puni
  :ensure t
  :after evil
  :hook
  ;; Enable puni-mode in Lisp modes
  (emacs-lisp-mode . puni-mode)
  (lisp-mode . puni-mode)
  :config
  (with-eval-after-load 'evil
    (evil-define-key '(normal insert) puni-mode-map
      (kbd "M-a")   'my/evil-insert-at-end-of-sexp
      (kbd "M-j")   'puni-end-of-sexp
      (kbd "M-k")   'puni-beginning-of-sexp)))

;; (use-package lispy
;;   :ensure t)
;; 
;; (use-package lispyville
;;   :ensure t
;;   :after (evil lispy))

(use-package highlight-sexp
  :ensure (:host github :repo "daimrod/highlight-sexp")
  :config
  (setq hl-sexp-background-color "#1c1c1c")
  :hook ((emacs-lisp-mode . highlight-sexp-mode)
         (lisp-mode . highlight-sexp-mode)
         (highlight-sexp-mode . (lambda () (hl-line-mode -1)))))

(use-package highlight-parentheses
  :ensure t
  :init
  ;; TODO: Move to the theme
  (defface my/hl-paren-innermost
    '((t :background "#5C4800" :foreground "#D7AF00"))
    "Innermost parenthesis in current scope."
    :group 'highlight-parentheses)

  (defface my/hl-paren-middle
    '((t :foreground "#d75f00" ))
    "Middle parenthesis in current scope."
    :group 'highlight-parentheses)
  
  
  :config
  ;; REVERSE ORDER: First = innermost, Last = outermost
  (setq highlight-parentheses-attributes
        '((:inherit my/hl-paren-innermost)    ; Level 1 - INNERMOST (closest to point)
          (:inherit my/hl-paren-middle)       ; Level 2
          (:inherit my/hl-paren-middle)       ; Level 3
          (:inherit my/hl-paren-middle)       ; Level 4
          (:inherit my/hl-paren-middle)))     ; TODO Level 5+ - OUTERMOST (farthest from point)
  
  ;; Clear the colors list (use attributes instead)
  (setq highlight-parentheses-colors nil)
  
  :hook ((emacs-lisp-mode . highlight-parentheses-mode)
         (lisp-mode . highlight-parentheses-mode)))

					; == Common Lisp Editing =======================================================
(use-package sly
  :ensure t
  :custom
  (inferior-lisp-proram "sbcl"))

(use-package sly-overlay
  :ensure t
  :after sly)

;; (defun my/sly-overlay-eval-last-expression ()
;;   (interactive)
;;   (sly-eval-async
;;       `(slynk:eval-and-grab-output
;; 	,(sly-last-expression))
;;     (lambda (result)
;;       (let ((value (cadr result)))
;; 	(sly-overlay--display-result value (point))))))


(provide 'lisp-editing)
;;; lisp-editing.el ends here (emacs-lisp-checkdoc)
