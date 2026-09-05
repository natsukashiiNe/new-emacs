;;; minibuffer-prompts.el --- Commands to be called from the system. -*- lexical-binding: t; -*-

;;; Commentary:
;; Those are some commands to help control the system.

;;; Code:


(require 'minibuffer-utils)


(cl-defun my-picker/make-picker (reader &key (width 100) (height 20)
					(name "vertico-pick"))
  "Run READER in a centered, WM-managed picker frame; return READER's value.
READER is a zero-arg function run with the picker frame focused.
The frame is deleted afterwards.

WIDTH is the width in columns of the created frame.
HEIGHT is the height in lines of the created frame.
NAME is the name for the WM to manage the window (X client / Emacs frame)."
  ;; TODO: remake it into picking the correct one depending on the active wm.
  (call-process "herbstclient" nil nil nil
		"rule" "once"
		(concat "title=" name)
		"floating=on"
		"focus=on")
  (let* ((frame (make-frame `((name       . ,name)
			      (title      . ,name)
			      (width      . ,width)
			      (height     . ,height)
			      (minibuffer . only))))
	 ;; TODO guard it
	 (vertico-posframe-mode nil))
    (unwind-protect
	(progn
	  ;; TODO: swap to WM agnostic func.
	  (select-frame-set-input-focus frame)
	  (my-picker--hc/center-frame frame)
	  (setq-local vertico-count height)
	  (funcall reader)
	  (redraw-frame frame))
      (when (frame-live-p frame)
	(delete-frame)))))

(defun my-driver/pick-ghostel-buffer ()
  "Pick a buffer, pre-filtered to \"ghostel\", via a centered picker frame."
  (my-picker/make-picker
   (lambda ()
     (minibuffer-with-setup-hook
         (lambda () (insert "ghostel"))
       (read-buffer "Buffer: " nil t)))))

;; bluetoothctl devices | sk | awk '{print $2}
(call-process "bluetoothctl" nil nil standard-output "devices")

(my-driver/pick-ghostel-buffer)


(cl-defun my-driver/create-minibuffer-frame ()
  (interactive)
  (call-process "herbstclient" nil nil nil
		"rule" "once"
		"title=driver-frame"
		"floating=on"
		"floatplacement=center"
		"focus=on")
  (let* ((driver-frame (make-frame
                        '((name . "driver-frame")
			  (title . "driver-frame")
                          (width . 100)
                          (height . 15)))))
    (select-frame-set-input-focus driver-frame)
    (my/execute-fn-with-prefix #'switch-to-buffer "ghostel")
    driver-frame))

(my-driver/create-minibuffer-frame)

(provide 'minibuffer-prompts)
;;; minibuffer-prompts.el ends here
