;;; telega-setup.el --- Telega configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Telegram client configuration.

;;; Code:
(require 'keymap-utils)

(use-package telega
  :ensure t
  :defer t
  :init
  ;; TODO does not work!
  ;; (defun my-telega-company-grab-emoji ()
  ;;   "Modified version that allows completion with just ':'."
  ;;   (let ((cg (company-grab "\\(?:^\\|[[:space:]]\\)\\(:[^: _]*\\)" 1
  ;;                           (- (point) telega-emoji-candidate-max-length))))
  ;;     (when cg (cons cg 0))))  ; NOTE: Changed to 0 to allow zero-length prefix

  ;; Override the original function
  :config
  (setq telega-emoji-use-images nil)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  ;; Suppress telega compilation warnings
  (with-eval-after-load 'comp
    (add-to-list 'native-comp-jit-compilation-deny-list "telega-.*\\.el$"))

  ;; overrides
  ;; (advice-add 'telega-company-grab-emoji :override #'my-telega-company-grab-emoji)
  :hook
  ((telega-root-mode . (lambda () (display-line-numbers-mode -1)))
   (telega-chat-mode . (lambda () (display-line-numbers-mode -1)))
   (telega-chat-mode .
                     (lambda ()
                       (require 'telega-company nil t)
                       (setq-local completion-at-point-functions
                         (append
                          (delq nil
                                (mapcar (lambda (fn)
                                          (when (fboundp fn)
                                            (cape-company-to-capf fn)))
                                        '(telega-company-botcmd
                                          telega-company-username
                                          telega-company-hashtag
                                          telega-company-emoji)))
                          completion-at-point-functions))))))

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
;; Use M-x my/telega-describe-pack to find sticker indices.
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
