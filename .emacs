(setq inferior-lisp-program "/usr/local/bin/sbcl")



(setq org-log-done 'time)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
;(add-to-list  'package-archives
;		'("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(require 'org)
(require 'ox-reveal)



(require 'haskell-interactive-mode)
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-hook 'erlang-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)
	    (global-auto-complete-mode)))

(global-auto-complete-mode)

(global-set-key (kbd "C-x C-g") #'git-command)
(global-set-key (kbd "C-x C-a") #'auto-complete-mode)
(global-set-key (kbd "C-x C-l") #'inferior-lfe)

(add-hook 'tuareg-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'utop-eval-phrase)
	    (global-auto-complete-mode)))

(add-hook 'slime-connected-hook
	  (lambda ()
	    (slime-load-file "/home/haetze/.packages/package.lisp")))




(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (ruby . t)
			     (java . t)
			     (haskell . t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("/home/haetze/Documents/Code/lfe/bin")))

(global-set-key [(control ?h)] 'delete-backward-char)
(gnus)
(calendar)
