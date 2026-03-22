;;; my-temp-layout-setup.el --- Layouting. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(requre 'cl-lib)

(cl-defstruct my-struct/layout
  name
  windows ; list of windows in the layout
  )

(cl-defun my-layout/last-or-new-vterm (&optional action)
  "Reuses last accesses vterm or creates a new one."
  (interactive)
  (let ((action (or action 'last)))
    (if (eq action 'new)
        (multi-vterm)
      ;; default: 'last — reuse last vterm if one exists, otherwise create new
      (if multi-vterm-buffer-list
          (switch-to-buffer (car multi-vterm-buffer-list))
        (multi-vterm)))))

(cl-defun my-layout/create-3pane-layout ()
  (interactive)
  (delete-other-windows)
  (let* ((main-window (selected-window))
         (side-panel     (split-window main-window -28 'left))
	 (bottom-panel   (split-window main-window -10 'below))
	 ;; (side-minipanel (split-window main-window -20 'right))
	 )
    ;; side-panel -> open dirvish
    (select-window side-panel)
    (projectile-dired)
    (hide-mode-line-mode 1)
    ;; bottom-panel -> open last-or-new vterm
    (select-window bottom-panel)
    (my-layout/last-or-new-vterm 'last)
    (hide-mode-line-mode 1)
    ;; return focus to main window
    (select-window main-window)))


(defvar-keymap my-layout-map
  :doc "Map to manage layout"
  "M" #'my-layout/create-3pane-layout)
(keymap-set global-map "C-c m" my-layout-map)

(keymap-set evil-normal-state-map "M-l" my-utility-normal-map)

(cl-defun my/layout-define)

(use-package window-purpose
  :config
  (purpose-mode))



(provide 'my-temp-layout-setup)
;;; my-temp-layout-setup.el ends here
