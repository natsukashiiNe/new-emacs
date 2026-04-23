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
      (kbd "L") #'dirvish-layout-switch))

  (keymap-set dired-mode-map "C-c C-c" 'dirvish-narrow)

  ;; == PREVIEW ============================================================

  (add-hook 'dirvish-special-preview-mode-hook
            (lambda () (display-line-numbers-mode -1))))

;; == LOAD SUB-MODULES ===================================================

(require 'sidebar-settings)
(require 'dired-scripts)

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
