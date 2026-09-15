;;; custom-macro.el --- Macroes I use in my config. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(defmacro l (&rest body)
  "Lamba convinience macro.  Executes BODY."
  `(lambda ()
     ,@body))

(defmacro li (&rest body)
  "Lamba interactive convinience macro.  Executes BODY."
  `(lambda ()
     (interactive)
     ,@body))

(defmacro ow (&rest body)
  "Interactive lambda that runs BODY under the \"other window\" prefix."
  `(lambda ()
     (interactive)
     (other-window-prefix)
     ,@body))

(defmacro of (&rest body)
  "Interactive lambda that runs BODY under the \"other window\" prefix."
  `(lambda ()
     (interactive)
     (other-frame-prefix)
     ,@body))

(defmacro ot (&rest body)
  "Interactive lambda that runs BODY under the \"other tab\" prefix."
  `(lambda ()
     (interactive)
     (other-tab-prefix)
     ,@body))

(defmacro fl (&rest body)
  `(funcall (lambda () ,@body)))

(defmacro fli (&rest body)
  `(funcall (lambda () (interactive) ,@body)))

(defmacro iput (expr)
  `(save-excursion
     (end-of-line)
     (insert "\n" (format "%S" ,expr))))

;; select-key: (select-key (:a 'a-def) map)
(defmacro k (key map)
  "Look up KEY in MAP via `map-elt' with a single letter macro for convienience.
KEY is either a bare a key, or a list of (KEY DEFAULT) for a deafult value."
  (if (and (consp key) (= (length key) 2))
      `(map-elt ,map ,(car key) ,(cadr key))
    `(map-elt ,map ,key)))

;; select-keys:  (select-keys [:a :b (:c 'c-def)] m)  — build a NEW plist of chosen keys
(defmacro ks (keys map)
  "Build a new plist from MAP containing KEYS and their values.
KEYS is a vector; each element is either a bare KEY or a list (KEY DEFAULT)."
  (let ((m (gensym "map")))
    `(let ((,m ,map))
       (list ,@(mapcan
                (l (spec)
                   (let ((key (if (consp spec) (car spec) spec)))
                     (list key `(k ,spec ,m))))
                (append keys nil))))))


(defmacro my/execute-fn-with-prefix (fn prefix)
  "Call FN interactively with PREFIX pre-inserted into the minibuffer.
FN may be given as a bare symbol (switch-to-buffer) or sharp-quoted
\(#'switch-to-buffer)."
  (let ((fn-sym (if (and (consp fn) (eq (car fn) 'function))
                    (cadr fn)
                  fn)))
    `(minibuffer-with-setup-hook
         (l (insert ,prefix))
       (command-execute #',fn-sym))))

(provide 'custom-macro)
;;; custom-macro.el ends here
