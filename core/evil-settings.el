;;; evil-settings.el --- Basic settings for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; General GUI settings, common packages and dependencies, qol.

;;; Code:

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  ;; adds "-" and "_" to treat as part of the word.
  (defun my/modify-word-syntax-with (symbol)
    "Treat SYMBOL as word constituent in current buffer's syntax table."
    (modify-syntax-entry symbol "w"))
  (defun my/modify-word-syntax-for-underscore ()
    "Treat underscore as word constituent."
    (modify-syntax-entry ?_ "w"))
  ;; todo: not used now
  (defun my/emacs-lisp-word-syntax ()
    "Treat underscore and hyphen as word constituents in Emacs Lisp."
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w"))

  :config
  (evil-mode 1)
  ;; KEYMAPS
  (dolist (map (list evil-normal-state-map evil-visual-state-map))
    (keymap-set map "C-u" 'evil-scroll-up))
  (dolist (map (list evil-insert-state-map minibuffer-mode-map))
    (keymap-set map "C-w" #'backward-kill-word)
    (keymap-set map "C-h" #'delete-backward-char))
  (keymap-set evil-normal-state-map "M-i" 'evil-switch-to-windows-last-buffer)
  (keymap-set evil-normal-state-map "M-h" 'tab-previous)
  (keymap-set evil-normal-state-map "M-l" 'tab-next)
  (global-set-key (kbd "C-x h") help-map)

  ;; hooks
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (send-string-to-terminal "\033]12;#FF8020\007")))
  
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (send-string-to-terminal "\033]12;#FF7F9F\007")))

  (add-hook 'prog-mode-hook #'my/modify-word-syntax-for-underscore))


;; Evil-escape
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "fj")
  (setq-default evil-escape-delay 0.15))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(provide 'evil-settings)
;;; evil-settings.el ends here

