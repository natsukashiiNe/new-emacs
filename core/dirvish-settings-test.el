;;; dirvish-settings.el --- Setup for dirvish. -*- lexical-binding: t; -*-

;;; Commentary:
;; Setting up dirvish.

;;; Code:

(use-package dirvish
  :ensure t
  :demand t
  :after (nerd-icons evil)
  :config
  (dirvish-override-dired-mode)
  ;; Ensure minimal fringe for terminal
  (when (not (display-graphic-p))
    (set-fringe-mode 1))  ; Minimal fringe in terminal

  (setq dirvish-attributes
        ;;'(nerd-icons file-info file-size vc-state git-msg))
        '(nerd-icons file-time file-size subtree-state))

  ;; Main listing: --all keeps . and .. visible
  ;; --group-directories-first puts directories on top
  (setq dired-listing-switches
        "-l --all --human-readable --group-directories-first --no-group")

  ;; Subtrees: --almost-all hides . and .. but keeps other dotfiles
  (setq dirvish-subtree-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  ;; == LAYOUT =========================================================
  ;; number of parents | max width of parent windows | width of preview
  (setq dirvish-header-line-height '(24 . 24))
  (setq dirvish-default-layout '(0 0.0 0.42))
  (setq dirvish-layout-recipes
        '((0 0 0.4)
          (0 0 0.8)
          (1 0.3 0.35)
          (1 0.11 0.55)))

  ;; == KEYMAPS =========================================================
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
      (kbd "C-k") #'dired-prev-dirline)
    )


  (keymap-set dired-mode-map "C-c C-c" 'dirvish-narrow)

  ;; Preview

  (add-hook 'dirvish-special-preview-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;;(dirvish-define-preview directory-custom (file ext preview-window)
  ;;   "Minimal directory preview with name, size, and date."
  ;;   :require nil
  ;;   (when (file-directory-p file)
  ;;     `(shell . ("bash" "-c"
  ;; 		 ,(concat
  ;;                  "ls -AhpS --group-directories-first "
  ;;                  "--time-style='+%Y-%m-%d %H:%M' "
  ;;                  (shell-quote-argument file)
  ;;                  " | awk '{print $6, $7, $5, $NF}'")))))

  ;; (setq dirvish-preview-dispatchers
  ;; 	'(directory-custom video image gif audio epub archive font pdf))
  ;; ;; (setq dirvish-mode-line-format
  ;;       '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; (setq dirvish-preview-side 'right)  ; or 'left
  ;; (setq dirvish-preview-size 0.5)     ; preview is 50% of frame width
  ;; ;; Enable file type dispatchers for preview
  ;; (setq dirvish-preview-dispatchers
  ;;       '(image gif video audio epub archive pdf))
  ;; (setq dirvish-default-layout '(0 0.4 0.6))  ; (window-min-height left-width right-width)
  )


(defun my/dirvish-fd-narrow ()
  "Run dirvish-fd in project root, then dirvish-narrow.
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

(with-eval-after-load 'dirvish
  ;; TODO Bind it to SPC f d
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC f"
   "d" '(my/dirvish-fd-narrow :which-key "dirvish fd+narrow")))

;; Navigation keybindings for dirvish-narrow minibuffer
(with-eval-after-load 'dirvish-narrow

  ;; Navigation helper functions
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

  ;; Custom keymap for dirvish-narrow
  (defvar my/dirvish-narrow-minibuffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-j") #'my/dirvish-narrow-next-line)
      (define-key map (kbd "C-k") #'my/dirvish-narrow-previous-line)
      (define-key map (kbd "C-d") #'my/dirvish-narrow-next-10-lines)
      (define-key map (kbd "C-u") #'my/dirvish-narrow-previous-10-lines)
      map)
    "Keymap active in minibuffer during `dirvish-narrow'.")

  ;; Inject keymap via advice
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

(provide 'dirvish-settings)
;;; dirvish-settings.el ends here
