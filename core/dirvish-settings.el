;;; dirvish-settings.el --- Setup for dirvish. -*- lexical-binding: t; -*-

;;; Commentary:
;; Setting up dirvish.

;;; Code:

(use-package dirvish
  :ensure t
  :after nerd-icons
  :init
  (dirvish-override-dired-mode)
  :config
  (add-hook 'dirvish-mode-hook
	    (l (display-line-numbers-mode -1)
	       (visual-line-mode -1)))

  ;; Enable preview on the side
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-preview-side 'right)  ; or 'left
  (setq dirvish-preview-size 0.5)     ; preview is 50% of frame width

  ;; Enable file type dispatchers for preview
  (setq dirvish-preview-dispatchers
        '(image gif video audio epub archive pdf))

  ;; Attributes to show (including preview)
  (setq dirvish-attributes
        '(nerd-icons file-info file-size vc-state git-msg)) 

  (setq dirvish-default-layout '(0 0.4 0.6))  ; (window-min-height left-width right-width)
  )



(provide 'dirvish-settings)
;;; dirvish-settings.el ends here
