;;; custom-editing.el --- Custom text editing functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom functions for text editing including:
;; - Copy previous word variants (with case toggle, avy selection)
;; - Increment/decrement numbers on line

;;; Code:

(require 'evil)
(require 'avy)

;; ----------------------------
;; Copy Previous Word Functions
;; ----------------------------

(defun my/copy-previous-word ()
  "Copy the previous word and paste it at point."
  (interactive)
  (let ((word (save-excursion
                (backward-word)
                (thing-at-point 'word t))))
    (if word
        (insert word)
      (user-error "No previous word found"))))

(defun my/toggle-first-char-case (str)
  "Toggle the case of the first character in STR."
  (if (and str (> (length str) 0))
      (let ((first-char (aref str 0))
            (rest (substring str 1)))
        (concat (char-to-string
                 (if (eq (upcase first-char) first-char)
                     (downcase first-char)
                   (upcase first-char)))
                rest))
    str))

(defun my/copy-previous-word-toggle-case ()
  "Copy the previous word, toggle case of first letter, and paste at point.
Useful for: MyClass -> myClass or myClass -> MyClass."
  (interactive)
  (let ((word (save-excursion
                (backward-word)
                (thing-at-point 'word t))))
    (if word
        (insert (my/toggle-first-char-case word))
      (user-error "No previous word found"))))

(defun my/avy-copy-word-backward ()
  "Use avy to select a word backward from cursor, copy from it to cursor position.
Words are enumerated from cursor backwards to beginning of line.
After selection, text from selected word to original cursor position is pasted."
  (interactive)
  (let* ((original-point (point))
         (line-beg (line-beginning-position))
         ;; Collect word positions backwards from cursor to line beginning
         (candidates (save-excursion
                       (let (words)
                         (goto-char original-point)
                         (while (and (> (point) line-beg)
                                     (re-search-backward "\\b\\w" line-beg t))
                           (push (point) words))
                         (nreverse words)))))
    (if (null candidates)
        (user-error "No words found before cursor on this line")
      ;; Use avy to select from candidates
      (let ((selected-pos (avy-process
                           (mapcar (lambda (pos)
                                     (cons pos (selected-window)))
                                   candidates))))
        (when selected-pos
          (let* ((start-pos (if (consp selected-pos) (car selected-pos) selected-pos))
                 (text-to-copy (buffer-substring-no-properties start-pos original-point)))
            (goto-char original-point)
            (insert text-to-copy)))))))

;; ----------------------------
;; Increment/Decrement Numbers
;; ----------------------------

(defun my/find-number-forward ()
  "Find the next number from point to end of line.
Returns (START . END) of the number or nil if not found."
  (save-excursion
    (when (re-search-forward "-?[0-9]+" (line-end-position) t)
      (cons (match-beginning 0) (match-end 0)))))

(defun my/find-number-backward ()
  "Find the previous number from beginning of line to point.
Returns (START . END) of the number or nil if not found."
  (save-excursion
    (let ((orig-point (point))
          (line-beg (line-beginning-position))
          result)
      (goto-char line-beg)
      ;; Find all numbers between line-beg and original point
      (while (re-search-forward "-?[0-9]+" orig-point t)
        (setq result (cons (match-beginning 0) (match-end 0))))
      result)))

(defun my/increment-number-at-region (start end delta)
  "Increment/decrement number between START and END by DELTA."
  (let* ((num-str (buffer-substring-no-properties start end))
         (num (string-to-number num-str))
         (new-num (+ num delta)))
    (delete-region start end)
    (goto-char start)
    (insert (number-to-string new-num))))

(defun my/change-number-on-line (delta)
  "Change first number on line by DELTA.
First searches forward from cursor to end of line.
If not found, searches backward from beginning of line to cursor.
DELTA is positive for increment, negative for decrement."
  (let ((forward-match (my/find-number-forward)))
    (if forward-match
        (my/increment-number-at-region (car forward-match) (cdr forward-match) delta)
      (let ((backward-match (my/find-number-backward)))
        (if backward-match
            (my/increment-number-at-region (car backward-match) (cdr backward-match) delta)
          (message "No number found on this line to %s"
                   (if (> delta 0) "increment" "decrement")))))))

(defun my/increment-number-at-point-or-line ()
  "Increment the first number found on the line.
Searches forward from cursor first, then backward from cursor."
  (interactive)
  (my/change-number-on-line 1))

(defun my/decrement-number-at-point-or-line ()
  "Decrement the first number found on the line.
Searches forward from cursor first, then backward from cursor.
Negative numbers become more negative (e.g., -5 becomes -6)."
  (interactive)
  (my/change-number-on-line -1))

(provide 'custom-editing)
;;; custom-editing.el ends here
