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
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)
	    (global-auto-complete-mode)))

(global-auto-complete-mode)

(global-set-key (kbd "C-x C-g") #'git-command)

(add-hook 'tuareg-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'utop-eval-phrase)
	    (global-auto-complete-mode)))

(add-hook 'slime-connected-hook
	  (lambda ()
	    (slime-load-file "/home/haetze/.packages/package.lisp")))

;;LFE mode.
;;(defvar lfe-dir (concat (getenv "HOME") "/git/lfe/emacs"))
;;(setq load-path (cons lfe-dir load-path))
;;(require 'lfe-start)

;;(global-set-key (kbd "<f12>") 'my-save-and-compile)
;;(defun my-save-and-compile ()
;;  (interactive "")
;;  (save-buffer 0)
;;  (compile "./run"))



(global-set-key "\C-h" 'delete-backward-char)
