;;; dirvish-setup.el --- Main dirvish configuration. -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for dirvish configuration.
;; Loads sidebar settings and custom scripts.

;;; Code:

(use-package dirvish
  :ensure t
  :demand t
  :after (nerd-icons evil)
  :config
  (dirvish-override-dired-mode)

  (when (not (display-graphic-p))
    (set-fringe-mode 1))

  (add-hook 'dired-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (hl-line-mode -1)
              (when (bound-and-true-p undo-tree-mode)
                (undo-tree-mode -1))))

  ;; == GENERAL DIRVISH SETTINGS ========================================

  (setq dirvish-use-mode-line nil)
  (setq dirvish-use-header-line nil)
  (setq dirvish-attributes '(file-size nerd-icons))

  (setq dired-listing-switches
        "-l --all --human-readable --group-directories-first --no-group")

  (setq dirvish-subtree-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  ;; == LAYOUT ===========================================================

  (setq dirvish-header-line-height '(24 . 24))
  (setq dirvish-default-layout '(0 0.0 0.5))
  (setq dirvish-layout-recipes
        '((0 0 0.4)
          (0 0 0.8)
          (1 0.3 0.35)
          (1 0.11 0.55)))

  ;; == KEYMAPS ===========================================================
  ;; TODO move to mode-keymaps.

  (defun my/dired-next-fileline (arg)
    "Move to the next ARG non-directory line in dired."
    (interactive "p")
    (let ((step (if (>= arg 0) 1 -1))
          (count (abs arg)))
      (while (> count 0)
        (let ((start (point))
              (found nil))
          (while (and (not found)
                      (zerop (forward-line step)))
            (let ((file (dired-get-filename nil t)))
              (when (and file (not (file-directory-p file)))
                (setq found t))))
          (if found
              (dired-move-to-filename)
            (goto-char start)
            (user-error "No more files"))
          (setq count (1- count))))))

  (defun my/dired-prev-fileline (arg)
    "Move to the previous ARG non-directory line in dired."
    (interactive "p")
    (my/dired-next-fileline (- arg)))

  (with-eval-after-load 'evil
    (evil-set-initial-state 'dirvish-mode 'normal)
    (evil-define-key 'normal dirvish-mode-map
      (kbd "h") #'dired-up-directory
      (kbd "l") #'dired-find-file
      (kbd "TAB") #'dirvish-subtree-toggle
      (kbd "C-g") #'dirvish-quit
      (kbd "q") #'dirvish-quit
      (kbd "S") #'dirvish-quicksort
      (kbd "C-j") #'dired-next-dirline
      (kbd "C-k") #'dired-prev-dirline
      (kbd "M-j") #'my/dired-next-fileline
      (kbd "M-k") #'my/dired-prev-fileline
      (kbd "C-n") #'my/dired-next-fileline
      (kbd "C-p") #'my/dired-prev-fileline
      (kbd "L") #'dirvish-layout-switch))

  (keymap-set dired-mode-map "C-c C-c" 'dirvish-narrow)

  ;; == PREVIEW ============================================================

  (add-hook 'dirvish-special-preview-mode-hook
            (lambda () (display-line-numbers-mode -1)))

  ;; == REDISPLAY GUARD ====================================================
  ;; Skip dirvish--render-attrs when the viewport hasn't changed. Without
  ;; this, every redisplay tears down and rebuilds all overlays for every
  ;; visible line, pegging CPU at 100% while idle. State is stored as a
  ;; window-parameter (not buffer-local) because the advice runs from
  ;; `redisplay--pre-redisplay-functions' where current-buffer can be any
  ;; buffer, not the dirvish one.
  (defun my/dirvish--skip-unchanged-render (orig-fn &optional window selected)
    (let* ((win (or window (selected-window)))
           (state (list (window-start win)
                        (window-end win)
                        (window-width win)
                        (window-point win)))
           (last (window-parameter win 'my/dirvish--last-render-state)))
      (unless (equal state last)
        (set-window-parameter win 'my/dirvish--last-render-state state)
        (funcall orig-fn window selected))))

  (advice-add 'dirvish--render-attrs :around #'my/dirvish--skip-unchanged-render)

  ;; Async VC/data lands after the initial render and triggers
  ;; `dirvish--redisplay'; without invalidating, the guard skips that
  ;; refresh because window geometry hasn't changed.
  (defun my/dirvish--invalidate-render-state ()
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (set-window-parameter win 'my/dirvish--last-render-state nil)))

  (add-hook 'dirvish-setup-hook #'my/dirvish--invalidate-render-state)
  (add-hook 'dirvish-after-revert-hook #'my/dirvish--invalidate-render-state)

  ;; == LOAD SUB-MODULES =================================================
  ;; Inside :config so they run after dirvish is installed/loaded by Elpaca.
  (require 'sidebar-settings)
  (require 'dired-scripts))

;; == KEYBINDINGS (after scripts loaded) ==================================

(with-eval-after-load 'general
  (with-eval-after-load 'dirvish
    (general-define-key
     :states '(normal visual)
     :keymaps 'override
     :prefix "SPC f"
     "d" '(my/dirvish-fd-narrow :which-key "dirvish fd+narrow"))))

(provide 'dirvish-setup)
;;; dirvish-setup.el ends here
