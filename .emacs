(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq package-archives '(("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
    ("http://johndcook.com/blog/feed" "https://michaelochurch.wordpress.com/feed/" "http://dave.cheney.net/feed/" "http://commitstripe.com/en/feed/" "http::/lunduke.com/wp-rss.php" "http://lambda-the-ultimate.org/rss.xml" "http://planet.haskell.org/rss20.xml" "http://fpcomplete.com/recent-content/feed" "http://blog.golang.org/feed.atom" "http://feeds.metaebene.me/freakshow/m4a" "http://www.bitsundso.de/feed/" "http://www.fanboys.fm/episodes.mp3.rss" "http://feeds.twit.tv/twig.xml" "http://feeds.metaebene.me/cre/m4a" "http://www.gamesundso.de/feed/" "http://feeds.feedburner.com/BsdNowMp3" "http://feeds.metaebene.me/lnp/m4a"
))))
(custom-set-faces ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'haskell-interactive-mode)
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-hook 'erlang-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-c") #'erlang-compile)))

(global-auto-complete-mode)


;;;Eshell Aliases*************************

;;(defalias 'xbat 'acpiconf -i 0 | grep "Remaining capacity"' 














