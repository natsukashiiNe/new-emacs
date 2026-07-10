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

  :custom
  ;; TODO: make it hook into my themes (light and dark)
  ;; cursor
  (evil-normal-state-cursor   '(box    "#990000"))
  (evil-insert-state-cursor   '(box    "#000000"))
  (evil-visual-state-cursor   '(box    "#0B5E30"))
  (evil-motion-state-cursor   '(box    "#0B5E30"))

  ;; ;;  (evil-visual-state-cursor  'box)
  ;; (evil-replace-state-cursor 'box)
  (evil-emacs-state-cursor   'box)

  :config

  (evil-mode 1)
  ;; KEYMAPS
  (dolist (map (list evil-normal-state-map evil-visual-state-map))
    (keymap-set map "C-u" 'evil-scroll-up))
  (dolist (map (list evil-insert-state-map minibuffer-mode-map))
    (keymap-set map "C-h" #'delete-backward-char))
  ;; C-w: delete word backward WITHOUT pushing to kill-ring.
  (defun my/delete-word-no-kill (arg)
    "Delete ARG words before point without touching the kill ring."
    (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point))))
  (dolist (map (list evil-insert-state-map minibuffer-mode-map))
    (keymap-set map "C-w" #'my/delete-word-no-kill))
  (keymap-set evil-normal-state-map "M-i" 'evil-switch-to-windows-last-buffer)
  (global-set-key (kbd "C-x h") help-map)

  ;; hooks
  ;; (add-hook 'evil-insert-state-exit-hook
  ;;           (lambda ()
  ;;             (when (not (display-graphic-p))
  ;; 		(send-string-to-terminal "\033]12;#FF8020\007"))))
  ;; (add-hook 'evil-insert-state-entry-hook
  ;;           (lambda ()
  ;;             (when (not (display-graphic-p))
  ;; 		(send-string-to-terminal "\033]12;#FF7F9F\007"))))

  (add-hook 'prog-mode-hook #'my/modify-word-syntax-for-underscore)

  ;; Make evil motions work with visual lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-visual-line)
  (define-key evil-motion-state-map "$" 'evil-end-of-visual-line)
  (define-key evil-motion-state-map "^" 'evil-first-non-blank-of-visual-line)
  (define-key evil-motion-state-map "_" 'evil-first-non-blank-of-visual-line)

  ;; For visual state as well
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
  ;; Visual block + $ needs logical EOL so each row extends to its own end.
  (define-key evil-visual-state-map "$" 'evil-end-of-line)
  )

(with-eval-after-load 'evil
  (setq evil-insert-state-cursor  '(box "#FF7F9F"))
  (setq evil-normal-state-cursor  '(box "#FF8020"))
  (setq evil-visual-state-cursor  '(box "#FF8020"))

  ;; `x' sends the deleted char to the kill-ring/clipboard by default.
  ;; Route it through the black-hole register instead.
  (evil-define-key 'normal 'global "x"
    (lambda (count) (interactive "p")
      (evil-delete-char (point) (+ (point) count) 'exclusive ?_)))
  (evil-define-key 'visual 'global "x"
    (lambda () (interactive)
      (evil-delete (region-beginning) (region-end) (evil-visual-type) ?_)))

  ;; Never pollute the kill-ring with whitespace-only deletions.
  ;; This covers `dd' on a blank line, `d$' on trailing spaces, etc.
  (defun my--evil-skip-whitespace-yank (orig beg end &optional register yank-handler)
    "Call ORIG unless the region BEG..END is purely whitespace."
    (unless (and (null register)
                 (string-blank-p (filter-buffer-substring beg end)))
      (funcall orig beg end register yank-handler)))
  (advice-add 'evil-yank-characters :around #'my--evil-skip-whitespace-yank)
  (advice-add 'evil-yank-lines      :around #'my--evil-skip-whitespace-yank))

;; Use the visual-state selection as the initial input for the next minibuffer
;; prompt (e.g. select a path, hit C-x C-f, get the path pre-filled).
(defvar my--visual-selection-for-minibuffer nil
  "Visual-state selection captured for the next minibuffer prompt.")

(defvar my-visual-to-minibuffer-excluded-commands
  '(evil-ex
    evil-ex-search-forward
    evil-ex-search-backward
    evil-ex-search-word-forward
    evil-ex-search-word-backward
    execute-extended-command)
  "Commands whose minibuffer should not be pre-filled with the region.
Evil-ex commands embed their own range markers (e.g. `\\='<,\\='>').")

(defun my--capture-visual-selection-for-minibuffer ()
  "Stash region text when invoking a command from evil visual state."
  (setq my--visual-selection-for-minibuffer
        (and (bound-and-true-p evil-local-mode)
             (eq evil-state 'visual)
             (not (memq this-command
                        my-visual-to-minibuffer-excluded-commands))
             (buffer-substring-no-properties
              (region-beginning) (region-end)))))

(defun my--insert-visual-selection-in-minibuffer ()
  "Replace minibuffer contents with the captured visual selection."
  (when my--visual-selection-for-minibuffer
    (let ((sel my--visual-selection-for-minibuffer))
      (setq my--visual-selection-for-minibuffer nil)
      (delete-minibuffer-contents)
      (insert sel))))

(add-hook 'pre-command-hook #'my--capture-visual-selection-for-minibuffer)
(add-hook 'minibuffer-setup-hook #'my--insert-visual-selection-in-minibuffer)

;; Evil-escape
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "fj")
  (setq-default evil-escape-delay 0.15)

  ;; keymap-set
  (evil-define-key '(insert visual) global-map (kbd "C-g") #'evil-escape))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

;; (use-package )
(use-package siege-mode
  :ensure (:host github :repo "tslilc/siege-mode")
  :demand t)

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(provide 'evil-settings)
;;; evil-settings.el ends here
