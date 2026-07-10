;;; elfeed-setup.el --- Short description -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for Elfeed - RSS Feed in Emacs and its supporting packages.


;;; Code:

(use-package elfeed
  :ensure t
  :defer t

  :config
  (setq elfeed-feeds
	'("http://nullprogram.com/feed/"
	  "https://planet.emacslife.com/atom.xml"

	  ("https://www.fluentcpp.com/feed/"           cpp blog)
          ("https://www.cppstories.com/index.xml"       cpp blog)
          ("https://www.modernescpp.com/index.php/feed/" cpp blog)
          ("https://herbsutter.com/feed/"               cpp blog)
          ("https://meetingcpp.com/feed.xml"            cpp news)
          ("https://isocpp.org/blog/rss"                cpp news)
          ("https://andreasfertig.com/blog/index.xml"   cpp blog)
          ("https://akrzemi1.wordpress.com/feed/"       cpp blog)
          ("https://shafik.github.io/feed.xml"          cpp blog)
          ("https://brevzin.github.io/feed.xml"         cpp blog)
          ("https://www.foonathan.net/index.xml"        cpp blog)
          ("https://preshing.com/feed"                  cpp blog)
          ("https://belaycpp.com/feed/"                 cpp blog)
          ("https://arne-mertz.de/feed/"                cpp blog))))


(provide 'elfeed-setup)
;;; elfeed-setup.el ends here
