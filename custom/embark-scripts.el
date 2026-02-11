;;; embark-scripts.el --- Collection of embark actions. -*- lexical-binding: t; -*-

;;; Commentary:
;; Collection of embark actions.

;;; Code:
(defun my-embark/eval-on-candidate (cand)
  "Eval an expression with CAND available."
  (interactive "sCandidate: ")
  (eval (read--expression "Eval (cand bound): ") t))

;; FIX: this does not work!
(defun my-embark/kill-lines (cand num dir)
  "Delete NUM lines at CAND. DIR is 'up' or 'down' when NUM > 1."
  (interactive
   (let* ((cand (read-string "Candidate: "))   ; <-- FIRST
          (num (read-number "Lines to delete: " 1))
          (dir (if (> num 1)
                   (intern (completing-read "Direction: " '("down" "up") nil t))
                 'down)))
     (list cand num dir)))
  (save-excursion
    (goto-char (point-max))  ; start from end to handle multiple deletions
    (when (search-backward cand nil t)
      (beginning-of-line)
      (when (and (eq dir 'up) (> num 1))
        (forward-line (- 1 num)))
      (kill-whole-line num))))

(keymap-set embark-general-map "E" 'my-embark/eval-on-candidate)
(keymap-set embark-general-map "K" 'my-embark/kill-lines)



(provide 'embark-scripts)
;;; embark-scripts.el ends here
