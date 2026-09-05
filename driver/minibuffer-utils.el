;;; minibuffer-utils.el --- Some utilities for my driver. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-picker/--kick-vertico ()
  "Force vertico to paint its candidate list once, in the active minibuffer."
  (when-let* ((win (active-minibuffer-window)))
    (with-selected-window win
      (when (bound-and-true-p vertico--input)
        (vertico--exhibit)))))

(cl-defun my-picker--hc/focused-monitor-rect()
  "Return (X Y W H) of the focused herbstluftwm monitor as integers."
  (mapcar #'string-to-number
	  (split-string
	   (string-trim
	    (with-output-to-string
	      (call-process "herbstclient" nil standard-output nil
			    "monitor_rect")))
	   " " t)))

(cl-defun my-picker--hc/center-frame (frame)
  "Center FRAME on the focused herbstluftwm monitor."
  (pcase-let* ((`(,mx ,my ,mw ,mh) (my-picker--hc/focused-monitor-rect))
	       (fw (frame-pixel-width frame))
	       (fh (frame-pixel-height frame)))
    (set-frame-position frame
			(+ mx (max 0 (/ (- mw fw) 2)))
			(+ my (max 0 (/ (- mh fh) 2)))))
  )

(provide 'minibuffer-utils)
;;; minibuffer-utils.el ends here
