;;; telega-setup.el --- Telega configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Telegram client configuration.

;;; Code:
(require 'keymap-utils)

(use-package telega
  :ensure t
  :defer t
  :custom
  ;; Names for reaction emoji missing from `etc/emojis.alist', so they
  ;; get a `:name:' annotation in the Add Reaction picker instead of
  ;; showing up bare. Many of these are the unqualified (no VS16)
  ;; codepoint used by Telegram's reaction list, whose qualified
  ;; sibling already has a name under a different key.
  (telega-emoji-custom-alist
   '((":heart-reaction:"       . "❤")
     (":smiling-face-with-hearts:" . "🥰")
     (":exploding-head:"       . "🤯")
     (":cursing-face:"         . "🤬")
     (":star-struck:"          . "🤩")
     (":vomiting-face:"        . "🤮")
     (":dove:"                 . "🕊")
     (":yawning-face:"         . "🥱")
     (":heart-on-fire:"        . "❤‍🔥")
     (":frankfurter:"          . "🌭")
     (":raised-eyebrow:"       . "🤨")
     (":man-technologist:"     . "👨‍💻")
     (":writing:"              . "✍")
     (":saluting-face:"        . "🫡")
     (":snowman-plain:"        . "☃")
     (":zany-face:"            . "🤪")
     (":moai:"                 . "🗿")
     (":man-shrugging:"        . "🤷‍♂")
     (":woman-shrugging:"      . "🤷‍♀")))
  :config
  (setq telega-emoji-use-images nil)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  ;; Suppress telega compilation warnings
  (with-eval-after-load 'comp
    (add-to-list 'native-comp-jit-compilation-deny-list "telega-.*\\.el$"))
  :hook
  ((telega-root-mode . (lambda () (display-line-numbers-mode -1)))
   (telega-chat-mode . (lambda () (display-line-numbers-mode -1)))
   ;; Native CAPF backends (telega-completions.el) replace the old
   ;; company->capf bridging; `t' appends them ahead of whatever
   ;; completion-at-point-functions chat buffers already had.
   (telega-chat-mode . (lambda () (telega-completions-setup-capf t)))))

;;; Quick-access helpers

(defun my/telega-send-sticker (pack-name index)
  "Send sticker at INDEX from PACK-NAME in current chatbuf."
  (let* ((sset (telega--searchStickerSet pack-name))
         (sticker (aref (plist-get sset :stickers) index)))
    (telega-chatbuf-sticker-insert sticker)))

(defun my/telega-react-emoji (emoji-string)
  "React to message at point with standard EMOJI-STRING."
  (let ((msg (telega-msg-at (point))))
    (when msg
      (telega--addMessageReaction
       msg (list :@type "reactionTypeEmoji" :emoji emoji-string)
       nil t))))

(defun my/telega-react-custom (custom-emoji-id)
  "React to message at point with CUSTOM-EMOJI-ID."
  (let ((msg (telega-msg-at (point))))
    (when msg
      (telega--addMessageReaction
       msg (list :@type "reactionTypeCustomEmoji"
                 :custom_emoji_id custom-emoji-id)
       nil t))))

(defun my/telega-reactions-missing-annotation ()
  "Report which emoji in `telega-emoji-reaction-list' have no name.
These are the ones that show up bare (no `:name:') in the
Add Reaction picker, since `telega-emoji-name' does a plain
string match against `telega-emoji-alist' (etc/emojis.alist)."
  (interactive)
  (unless telega-emoji-reaction-list
    (user-error "telega-emoji-reaction-list is empty; open a chat first"))
  (let ((missing (seq-remove #'telega-emoji-name telega-emoji-reaction-list)))
    (with-help-window "*Telega Missing Reaction Annotations*"
      (princ (format "%d/%d reactions have no annotation:\n\n"
                     (length missing) (length telega-emoji-reaction-list)))
      (dolist (e missing)
        (princ (format "%s  (U+%s)\n" e
                       (mapconcat (lambda (c) (format "%X" c)) e " ")))))))

(defun my/telega-describe-pack (pack-name)
  "Show stickers in PACK-NAME with their indices and custom emoji IDs.
Use this to discover values for quick-access keymaps."
  (interactive "sPack short name (from t.me/addstickers/NAME): ")
  (let* ((sset (telega--searchStickerSet pack-name))
         (stickers (plist-get sset :stickers)))
    (with-help-window "*Telega Pack*"
      (princ (format "Pack: %s (%s)\nStickers: %d\n\n"
                     (plist-get sset :title) pack-name
                     (length stickers)))
      (dotimes (idx (length stickers))
        (let* ((sticker (aref stickers idx))
               (emoji-id (telega--tl-get sticker :full_type
                                         :custom_emoji_id)))
          (princ (format "[%d] emoji: %s  custom-emoji-id: %s\n"
                         idx
                         (plist-get sticker :emoji)
                         (or emoji-id "N/A"))))))))

;;; Quick-access sticker keymap: C-c g <key>
(defvar-keymap my-maps/telega-quick-sticker
  :doc "Quick access to stickers")

(my-keymaps/populate my-maps/telega-quick-sticker
  ("m"   "muvluv")
  ("m s" (li (my/telega-send-sticker "muvluv" 0))    "Sumika cutie")
  ("m S" (li (my/telega-send-sticker "muvluv" 1))    "Sumika happy")
  ("b"   "badebils")
  ("b b" (li (my/telega-send-sticker "badebils" 86)) "Yuki not-happy"))

;; custom emoji ID get from M-x my/telega-describe-pack
(defvar-keymap my-maps/telega-quick-reactions
  :doc "Quick access to reactions")

(my-keymaps/populate my-maps/telega-quick-reactions
  ("y" (li (my/telega-react-custom "5269427257281119448")) "[Y]uki not-happy")
  ("f" (li (my/telega-react-custom "5269688326868207767")) "Make love")
  ("w" (li (my/telega-react-custom "5269294310863437649")) "[W]a!")
  ("b" (li (my/telega-react-custom "5269482361711525480")) "[B]anana")
  ;; Default ones
  ("l" (li (my/telega-react-emoji "👍")) "thumbs-up")
  ("h" (li (my/telega-react-emoji "❤")) "heart")
  ("f" (li (my/telega-react-emoji "🔥")) "fire"))

(with-eval-after-load 'evil
  (evil-define-key 'normal telega-chat-mode-map
    (kbd "u")   #'telega-chatbuf-next-unread
    (kbd "C-r") #'telega-chatbuf-next-unread-reaction
    (kbd "G")   #'telega-chatbuf-read-all
    (kbd "m")   #'telega-chatbuf-next-unread-mention
    (kbd "T")   #'telega-chatbuf-filter-by-topic
    (kbd "C-k") #'telega-msg-previous
    (kbd "C-j") #'telega-msg-next

    (kbd "C-c RET") #'telega-chatbuf-input-send)

  (evil-define-key 'insert telega-chat-mode-map
    (kbd "C-c RET") #'telega-chatbuf-input-send))


(with-eval-after-load 'telega-chat
  (define-key telega-chat-mode-map (kbd "C-c g") my-maps/telega-quick-sticker)
  (define-key telega-chat-mode-map (kbd "C-c G") my-maps/telega-quick-reactions))

(provide 'telega-setup)
;;; telega-setup.el ends here
