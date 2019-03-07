(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
;;install packages needed
(defun install-package (pkg)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

(setq my-packages (list
		   'use-package
		   'rust-mode
		    'git-command
		    'haskell-mode
		    'idris-mode 
		    'org-plus-contrib 
		    'org-ref 
		    'w3m))

(dolist (pkg my-packages)
  (install-package pkg))
      
;;requires and configuration
(require 'org)
(require 'org-ref)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(use-package org
  :ensure org-plus-contrib)

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Contacts/Private.org"
				"~/Contacts/Uni.org")))

(defvar my/org-contacts-template "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{EMPTY}
:BIRTHDAY: %^{BIRTHDAY}t
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")



(defvar code-template "* %^{NAME} 
#+BEGIN_src %^{LANGUAGE} 
%c
#+END_src")


(use-package org-capture
  :ensure nil
  :after org
  :preface
  :custom
  (org-capture-templates
   `(("c" "Contact" entry (file+headline "~/Contacts/Private.org" "Contacts"),
      my/org-contacts-template
      :empty-lines 1)
     ("C" "Contact" entry (file+headline "~/Contacts/Uni.org" "Contacts"),
      my/org-contacts-template
      :empty-lines 1)
     ("t" "TODOs in tasks.org" entry (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME} \n SCHEDULED: %^{SCHEDULED?}T")
     ("m" "TODOs in tasks.org" entry (file+headline "~/TODOS/tasks.org" "Mail")
      "* TODO %? , Link: %a")
     ("r" "TODOs in tasks.org" entry (file+headline "~/TODOS/tasks.org" "Mail")
      "* TODO Reply %a")
     ("s" "SRCs in Code.org" entry (file+headline "~/TODOS/Code.org" "Code"),
      code-template)
   )))



(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'org-latex-classes
           '("book-noparts"
              "\\documentclass{book}"
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; variable setting
(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("/home/haetze/Documents/Code/lfe/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/.cabal/bin"))
(setq exec-path (append exec-path '("/home/haetze/.cabal/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/.cargo/bin"))
(setq exec-path (append exec-path '("/home/haetze/.cargo/bin")))

(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))
(setq org-log-done 'time)

(setq show-paren-style 'expression)

(global-set-key (kbd "C-x C-g") #'git-command)
(global-set-key (kbd "C-o") #'open-in-firefox-direct)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c i") #'org-insert-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x C-a") #'auto-complete-mode)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "M-s C-l") #'cycle-languages)
(global-set-key [(control ?h)] 'delete-backward-char)
(global-set-key (kbd "C-x c f") #'column-enforce-mode)



(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (ruby . t)
			     (java . t)
			     (haskell . t)))

(setq inhibit-startup-screen t)
(setq european-calendar-style 't)
(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)

(defun write-mode (lang)
  (interactive "sLang:")
  (auto-complete-mode)
  (column-enforce-mode)
  (flyspell-mode)
  (ispell-change-dictionary lang))

(defun de-write-mode ()
  (interactive)
  (write-mode "deutsch"))

(defun en-write-mode ()
  (interactive)
  (write-mode "english"))


(defun open-in-firefox (url)
  (interactive "sURL: ")
  (delete-window (shell-command (concat "firefox \"" url "\" &"))))

(defun open-in-firefox-direct ()
  (interactive)
  (w3m-print-this-url t)
  (let ((url (car kill-ring)))
    (setq kill-ring (cdr kill-ring))
    (open-in-firefox url)))


;;For Spell Checking
;;Toogle Languages English-German, German-English
(let ((langs '("deutsch" "english")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))



(defun compile-lfe-module ()
  (interactive)
  (comint-send-string (inferior-lfe-proc) (concat "(c '" (substring buffer-file-name 0 -4) ")\n")))


(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'erlang-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)))    
(add-hook 'lfe-mode-hook 'highlight-parentheses-mode)
(add-hook 'lfe-mode-hook (lambda ()
			   (local-set-key (kbd "C-x C-l") #'inferior-lfe)
			   (local-set-key (kbd "C-c C-c") #'compile-lfe-module)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-chromium-program "firefox")
 '(custom-enabled-themes (quote (manoj-dark)))
 '(org-agenda-files (quote ("~/Contacts/Uni.org" "~/Contacts/Private.org" "~/TODOS/tasks.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


