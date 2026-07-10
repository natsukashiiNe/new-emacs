;;; custom-editing.el --- Custom text editing functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom functions for text editing including:
;; - Copy previous word variants (with case toggle, avy selection)
;; - Increment/decrement numbers on line

;;; Code:

(require 'cl-lib)
(declare-function avy-process "avy" (candidates &optional overlay-fn cleanup-fn))

;; ----------------------------
;; Copy Previous Word Functions
;; ----------------------------

(defun my-edit/copy-previous-word ()
  "Copy the previous word and paste it at point."
  (interactive)
  (let ((word (save-excursion
                (backward-word)
                (thing-at-point 'word t))))
    (if word
        (insert word)
      (user-error "No previous word found"))))

(defun my-edit/toggle-first-char-case (str)
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

(defun my-edit/copy-previous-word-toggle-case ()
  "Copy the previous word, toggle case of first letter, and paste at point.
Useful for: MyClass -> myClass or myClass -> MyClass."
  (interactive)
  (let ((word (save-excursion
                (backward-word)
                (thing-at-point 'word t))))
    (if word
        (insert (my-edit/toggle-first-char-case word))
      (user-error "No previous word found"))))

(defun my-edit/avy-copy-word-backward ()
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

(defun my-edit/increment-number-at-point-or-line ()
  "Increment the first number found on the line.
Searches forward from cursor first, then backward from cursor."
  (interactive)
  (my/change-number-on-line 1))

(defun my-edit/decrement-number-at-point-or-line ()
  "Decrement the first number found on the line.
Searches forward from cursor first, then backward from cursor.
Negative numbers become more negative (e.g., -5 becomes -6)."
  (interactive)
  (my/change-number-on-line -1))

;; ------------------------------------
;; Comment Line Insertion
;; ------------------------------------

(defvar my/edit--comm-symbols-completion '("=" "-" "*" "#")
  "Symbols available for comment line fill.")

(cl-defun my/edit--insert-comment (&key (symbol "=") (num-before 2) (length 80) dir text)
  "Insert a formatted comment separator line.
SYMBOL is the fill character (single char string).
NUM-BEFORE is how many symbols to place before TEXT.
LENGTH is the target line width (last column to contain a symbol).
DIR is \\='top, \\='bottom, or \\='in-place.
TEXT is the label to embed in the comment line.

Example output (C++ with 4-space indent):
    // == SECTION HEADER ======================================================="
  (let* ((indent (current-indentation))
         (comment (string-trim-right (or comment-start "#")))
         (indent-str (make-string indent ?\s))
         (sym-char (if (> (string-width symbol) 0) (aref symbol 0) ?=))
         (has-text (and text (not (string-empty-p text))))
         ;; Build prefix: indent + comment + space + num-before symbols
         (before-syms (make-string num-before sym-char))
         (prefix (if has-text
                     (concat indent-str comment " " before-syms " ")
                   (concat indent-str comment " ")))
         ;; Build text part with trailing space
         (text-part (if has-text (concat text " ") ""))
         ;; Calculate fill
         (used (+ (string-width prefix) (string-width text-part)))
         (fill-count (max 0 (- length used)))
         (fill-str (make-string fill-count sym-char))
         (line (concat prefix text-part fill-str)))
    ;; Position cursor based on DIR
    (pcase dir
      ('top
       (beginning-of-line)
       (open-line 1)
       (delete-region (line-beginning-position) (line-end-position)))
      ('bottom
       (end-of-line)
       (newline))
      ('in-place
       (beginning-of-line)
       (delete-region (line-beginning-position) (line-end-position))))
    (insert line)))

(cl-defun my/edit--insert-centered-comment (&key (symbol "=") (length 80) dir text)
  "Insert a centered comment separator line.
Calculates NUM-BEFORE to center TEXT within the line.
SYMBOL, LENGTH, DIR, TEXT as in `my/edit--insert-comment'."
  (let* ((indent (current-indentation))
         (comment (string-trim-right (or comment-start "#")))
         ;; Available space for symbols: length - indent - comment - spaces
         ;; Layout: indent + comment + " " + syms + " " + TEXT + " " + syms
         (has-text (and text (not (string-empty-p text))))
         (fixed-width (+ indent (string-width comment) 1)) ; indent + comment + space
         (text-width (if has-text (+ (string-width text) 2) 0)) ; spaces around text
         (available (max 0 (- length fixed-width text-width)))
         (num-before (/ available 2)))
    (my/edit--insert-comment
     :symbol symbol :num-before num-before :length length :dir dir :text text)))

(defmacro my/edit--def-comment-fn (base-name docstring &rest body)
  "Generate three defuns from BASE-NAME: in-place, --top, --bottom.
DOCSTRING is the base documentation. BODY should reference `dir'
and call `my/edit--insert-comment' or `my/edit--insert-centered-comment'."
  (let ((fn-inplace (intern (symbol-name base-name)))
        (fn-top     (intern (concat (symbol-name base-name) "--top")))
        (fn-bottom  (intern (concat (symbol-name base-name) "--bottom"))))
    `(progn
       (defun ,fn-inplace ()
         ,(concat docstring "\nInserts in-place on the current line.")
         (interactive)
         (let ((dir 'in-place)) ,@body))
       (defun ,fn-top ()
         ,(concat docstring "\nInserts on a new line above.")
         (interactive)
         (let ((dir 'top)) ,@body))
       (defun ,fn-bottom ()
         ,(concat docstring "\nInserts on a new line below.")
         (interactive)
         (let ((dir 'bottom)) ,@body)))))

;; -- Full-options comment (prompts for all params) --

(my/edit--def-comment-fn my/edit-insert-comment
			 "Insert a comment separator line, prompting for all options."
			 (let ((symbol (completing-read "Symbol: " my/edit--comm-symbols-completion nil nil "="))
			       (num-before (read-number "Num symbols before text: " 2))
			       (length (read-number "Line length: " 80))
			       (text (read-string "Text: ")))
			   (my/edit--insert-comment
			    :symbol symbol :num-before num-before :length length :dir dir :text text)))

;; -- Default comment (prompts only for text) --

(my/edit--def-comment-fn my/edit-insert-comment-default
			 "Insert a comment separator line with default settings."
			 (let ((text (read-string "Text: ")))
			   (my/edit--insert-comment
			    :symbol "=" :num-before 2 :length 80 :dir dir :text text)))

;; -- Centered comment (prompts for symbol, length, text) --

(my/edit--def-comment-fn my/edit-insert-centered-comment
			 "Insert a centered comment separator line, prompting for options."
			 (let ((symbol (completing-read "Symbol: " my/edit--comm-symbols-completion nil nil "="))
			       (length (read-number "Line length: " 80))
			       (text (read-string "Text: ")))
			   (my/edit--insert-centered-comment
			    :symbol symbol :length length :dir dir :text text)))

;; -- Centered default comment (prompts only for text) --

(my/edit--def-comment-fn my/edit-insert-centered-comment-default
			 "Insert a centered comment separator line with default settings."
			 (let ((text (read-string "Text: ")))
			   (my/edit--insert-centered-comment
			    :symbol "=" :length 80 :dir dir :text text)))

;; ------------------------------------
;; Org Table to Bullet List
;; ------------------------------------

(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function org-table-begin "org-table" (&optional table-type))
(declare-function org-table-end "org-table" (&optional table-type))

(defun my/edit-org-table-to-bullets ()
  "Convert the org table at point into a nested bullet list.

First column becomes top-level bullets (`-'), labeled with the first
column's header name. Remaining columns become sub-bullets (`+'),
labeled with their header names padded so the colons align to the
widest header. Each sub-bullet value is terminated with a period.

Replaces the table in place. Signals an error if point is not inside
an org table. Separator rows (|---...) are ignored."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not inside an org table"))
  (let* ((rows (cl-remove-if (lambda (r) (eq r 'hline))
                             (org-table-to-lisp)))
         (header (car rows))
         (data-rows (cdr rows))
         (_ (unless data-rows
              (user-error "Table has no data rows")))
         (first-col-name (string-trim (car header)))
         (sub-col-names (mapcar #'string-trim (cdr header)))
         (max-width (apply #'max (mapcar #'string-width sub-col-names)))
         (table-beg (org-table-begin))
         (table-end (org-table-end))
         (output
          (mapconcat
           (lambda (row)
             (let ((first-val (string-trim (car row)))
                   (sub-vals (cdr row)))
               (concat
                (format "- %s: %s\n" first-col-name first-val)
                (mapconcat
                 (lambda (pair)
                   (let* ((name (car pair))
                          (val (string-trim (cdr pair)))
                          (pad (make-string
                                (- max-width (string-width name)) ?\s))
                          (val-with-dot
                           (if (or (string-empty-p val)
                                   (string-match-p "[.!?]\\'" val))
                               val
                             (concat val "."))))
                     (format "  + %s%s: %s" name pad val-with-dot)))
                 (cl-mapcar #'cons sub-col-names sub-vals)
                 "\n"))))
           data-rows
           "\n")))
    (goto-char table-beg)
    (delete-region table-beg table-end)
    (insert output "\n")))

;; ------------------------------------
;; Insert From *Messages* Buffer
;; ------------------------------------

(defun my-edit--message-lines ()
  "Return the non-empty lines of the *Messages* buffer.
Lines are returned oldest-first (buffer order)."
  (with-current-buffer (messages-buffer)
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n" t)))

(defun my-edit--insert-message-line (line)
  "Insert LINE at point.
Shared helper for the interactive message-insertion commands."
  (when (and line (not (string-empty-p line)))
    (insert line)))

(defun my-edit/insert-last-message-line ()
  "Insert the last line from the *Messages* buffer at point."
  (interactive)
  (let ((line (car (last (my-edit--message-lines)))))
    (if line
        (my-edit--insert-message-line line)
      (user-error "No messages to insert"))))

(defun my-edit/pick-insert-message-line ()
  "Pick a line from the *Messages* buffer via completion and insert it.
Candidates are presented newest-first."
  (interactive)
  (let ((lines (nreverse (my-edit--message-lines))))
    (if (null lines)
        (user-error "No messages to insert")
      (my-edit--insert-message-line
       (completing-read "Message line: " lines nil t)))))

;; ------------------------------------
;; Insert File Path
;; ------------------------------------

(defun my-edit--insert-file-path (&optional path)
  "Read a file name and insert it at point.
PATH pre-fills the prompt: a directory sets the starting directory,
while a full file path also pre-fills the file-name part.  With no
PATH, behaves like the usual `find-file' starting in `default-directory'."
  (let* ((path (or path default-directory))
         (dir (file-name-directory path))
         (initial (file-name-nondirectory path)))
    (insert (read-file-name "Insert file path: " dir nil nil
                            (and (not (string-empty-p initial)) initial)))))

(defmacro my-edit--make/insert-file-path (path &rest args)
  "Define an interactive command that inserts a file path at point.
PATH pre-fills the prompt (see `my-edit--insert-file-path').  ARGS is a
plist; :name gives the command name (a string or symbol).

Example:
  (my-edit--make/insert-file-path
   \"~/projects/\" :name \"my-edit/insert-project-path\")"
  (let* ((name (plist-get args :name))
         (fn (if (stringp name) (intern name) name)))
    (unless fn
      (error "my-edit--make/insert-file-path: missing :name"))
    `(defun ,fn ()
       ,(format "Insert a file path at point%s."
                (if path (format ", pre-filled with %S" path) ""))
       (interactive)
       (my-edit--insert-file-path ,path))))

;; Plain version: usual `find-file' behavior of the current directory.
(my-edit--make/insert-file-path nil :name "my-edit/insert-file-path")
(my-edit--make/insert-file-path "~/Pictures/screenshots/"
				:name "my-edit/insert-screenshot-path")

;; ------------------------------------
;; Block Editing (Change inside brackets)
;; ------------------------------------

(require 'misc) ; zap-up-to-char

(defun my-edit--change-inner-pair (open close)
  "Delete content inside the nearest enclosing OPEN/CLOSE pair.
Climbs up bracket levels until the right OPEN char is found."
  (condition-case nil
      (let (start end)
        (save-excursion
          (let (found)
            (while (not found)
              (up-list -1 t t)
              (when (eq (char-after) open)
                (setq found t start (point)))))
          (forward-sexp)
          (setq end (1- (point))))
        (delete-region (1+ start) end)
        (goto-char (1+ start)))
    (error (user-error "No enclosing %c...%c found" open close))))

(defun my-edit/change-inner-any-bracket ()
  "Delete content inside the nearest enclosing bracket pair (any of ([{)."
  (interactive)
  (condition-case nil
      (let* ((close-pos (save-excursion (up-list 1 t t) (point)))
             (open-pos  (save-excursion (up-list -1 t t) (point))))
        (delete-region (1+ open-pos) (1- close-pos))
        (goto-char (1+ open-pos)))
    (error (user-error "Not inside a bracket pair"))))

(defun my-edit/change-inner-paren ()
  "Delete content inside nearest ()."
  (interactive) (my-edit--change-inner-pair ?\( ?\)))

(defun my-edit/change-inner-bracket ()
  "Delete content inside nearest []."
  (interactive) (my-edit--change-inner-pair ?\[ ?\]))

(defun my-edit/change-inner-curly ()
  "Delete content inside nearest {}."
  (interactive) (my-edit--change-inner-pair ?\{ ?\}))

(defun my-edit/delete-to-char-backward (char)
  "Delete from point back to (but not including) CHAR."
  (interactive "cDelete back to char: ")
  (zap-up-to-char -1 char))

(provide 'custom-editing)
;;; custom-editing.el ends here
