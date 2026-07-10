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

;; get / map-as-function:  (m :k)  (m :k default)   — single level, setf-able
(defmacro k (map key &optional default)
  `(map-elt ,map ,key ,default))

;; select-keys:  (select-keys m [:a :b])  — build a NEW plist of chosen keys
(defmacro ks (map &rest keys)
  (let ((m (make-symbol "m")))
    `(let ((,m ,map))
       (list ,@(apply #'append
                      (mapcar (lambda (key) (list key `(map-elt ,m ,key)))
                              keys))))))

(provide 'custom-macro)
;;; custom-macro.el ends here
