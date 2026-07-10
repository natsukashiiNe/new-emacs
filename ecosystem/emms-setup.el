;;; emms-setup.el --- EMMS is the Emacs Multimedia System. -*- lexical-binding: t; -*-

;;; Commentary:
;; Emms is the Emacs Multimedia System.  Emms displays and plays
;; multimedia from within GNU/Emacs using a variety of external players
;; and from different sources.

;; Emms can run as a minimalist player and controlled with a handful of
;; M-x Emacs commands, or a fully-fledged, interactive media
;; browser.  Emms can display album art, play streaming audio, tag music
;; files, search for lyrics, provide MPD connectivity, control the
;; volume, and more.

;;; Code:

(use-package emms
  :ensure t
  :defer t)


(provide 'emms-setup)
;;; emms-setup.el ends here
