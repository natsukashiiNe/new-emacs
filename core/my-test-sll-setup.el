(cl-defun my--create-new-window (&key dir size ratio)
  (when (and size ratio)
    (user-error "my--create-new-window: provide either :size or :ratio, not both"))
  (let ((actual-size (or size
                         (round (* (frame-width) (or ratio 0.35))))))
    ))

(defun my-create-layout ()
  (split-root-window-right ))


(fl (let* ((initial-win (selected-window))
           (new-win (split-root-window-right -80)))
      (set-window-parameter initial-win 'test-parameter "initial-win")
      (set-window-parameter new-win 'test-parameter "new-win")))
