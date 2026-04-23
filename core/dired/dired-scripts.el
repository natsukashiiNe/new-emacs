;;; scripts.el --- Custom dirvish scripts. -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom scripts for dirvish-narrow and other utilities.

;;; Code:

(require 'dirvish)

;; Prevent dirvish from re-rendering overlays when viewport hasn't changed.
;; Without this, dirvish--render-attrs runs on every pre-redisplay event
;; (tears down and rebuilds all overlays), consuming ~69% CPU while idle.
(defvar-local my/dirvish--last-render-state nil)

(defun my/dirvish--skip-unchanged-render (orig-fn &optional window)
  "Skip dirvish attribute re-rendering when viewport hasn't changed."
  (let* ((win (or window (selected-window)))
         (state (list (window-start win) (window-end win t) (window-width win)
                      (with-selected-window win (point)))))
    (unless (equal state my/dirvish--last-render-state)
      (setq my/dirvish--last-render-state state)
      (funcall orig-fn window))))

(advice-add 'dirvish--render-attrs :around #'my/dirvish--skip-unchanged-render)

(defun my/dirvish-fd-narrow ()
  "Run `dirvish-fd' in project root, then `dirvish-narrow'.
If already in a project, use current project root.
Otherwise, prompt to select a project first."
  (interactive)
  (let* ((current-project (projectile-project-root))
         (project-action (lambda ()
                           (let ((project-root (projectile-project-root)))
                             (dirvish-fd project-root "")
                             (dirvish-narrow)))))
    (if current-project
        (funcall project-action)
      (let ((projectile-switch-project-action project-action))
        (condition-case nil
            (call-interactively 'projectile-switch-project)
          (quit nil))))))

;; Navigation keybindings for dirvish-narrow minibuffer
(with-eval-after-load 'dirvish-narrow

  (defun my/dirvish-narrow-next-line (&optional n)
    "Move down N lines (default 1) in dirvish buffer during narrow."
    (interactive "p")
    (when-let* ((win (minibuffer-selected-window)))
      (with-selected-window win
        (dired-next-line (or n 1)))))

  (defun my/dirvish-narrow-previous-line (&optional n)
    "Move up N lines (default 1) in dirvish buffer during narrow."
    (interactive "p")
    (when-let* ((win (minibuffer-selected-window)))
      (with-selected-window win
        (dired-previous-line (or n 1)))))

  (defun my/dirvish-narrow-next-10-lines ()
    "Move down 10 lines in dirvish buffer during narrow."
    (interactive)
    (my/dirvish-narrow-next-line 10))

  (defun my/dirvish-narrow-previous-10-lines ()
    "Move up 10 lines in dirvish buffer during narrow."
    (interactive)
    (my/dirvish-narrow-previous-line 10))

  (defvar my/dirvish-narrow-minibuffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-j") #'my/dirvish-narrow-next-line)
      (define-key map (kbd "C-k") #'my/dirvish-narrow-previous-line)
      (define-key map (kbd "C-d") #'my/dirvish-narrow-next-10-lines)
      (define-key map (kbd "C-u") #'my/dirvish-narrow-previous-10-lines)
      map)
    "Keymap active in minibuffer during `dirvish-narrow'.")

  (defun my/dirvish-narrow-activate-custom-keymap ()
    "Activate navigation keymap for dirvish-narrow minibuffer."
    (use-local-map
     (make-composed-keymap my/dirvish-narrow-minibuffer-map
                           (current-local-map))))

  (advice-add 'dirvish-narrow :around
              (lambda (orig-fn &rest args)
                "Add custom navigation keybindings during dirvish-narrow."
                (minibuffer-with-setup-hook
                    #'my/dirvish-narrow-activate-custom-keymap
                  (apply orig-fn args)))))

(provide 'dired-scripts)
;;; scripts.el ends here
