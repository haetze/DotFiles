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
		    'ess
		    'openwith
		    'w3m))

(dolist (pkg my-packages)
  (install-package pkg))
      
;;requires and configuration
(require 'org)
(require 'org-tempo)
(require 'org-ref)
(require 'ox-extra)

(ox-extras-activate '(ignore-headlines))

(add-to-list 'load-path "~/.emacs.d/template/")
(setq org-latex-to-pdf-process (list "latexmk -pdf %f"))


(require 'template)
(template-initialize)

(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "xpdf" (file))))

(use-package org
  :ensure org-plus-contrib)

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Contacts/Private.org"
				"~/Contacts/Uni.org")))

(defvar my/org-contacts-template
"* %^{NAME}
:PROPERTIES:
:ADDRESS: %^{EMPTY}
:BIRTHDAY: %^{BIRTHDAY}t
:EMAIL: %^{EMAIL}
:PHONE: %^{PHONE}
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")



(defvar code-template
"* %^{NAME}                       
#+BEGIN_src %^{LANGUAGE} 
%c
#+END_src")


(use-package org-capture
  :ensure nil
  :after org
  :preface
  :custom
  (org-capture-templates
   `(("c" "Contact Private" entry (file+headline "~/Contacts/Private.org" "Contacts"),
      my/org-contacts-template
      :empty-lines 1)
     ("C" "Contact Uni" entry (file+headline "~/Contacts/Uni.org" "Contacts"),
      my/org-contacts-template
      :empty-lines 1)
     ("t" "TODOs in tasks.org (Scheduled/Deadline)" entry (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t\nDEADLINE: %^{DEADLINE?}t")
     ("D" "TODOs in tasks.org (Deadline)" entry (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME}\nDEADLINE: %^{DEADLINE?}t")
     ("S" "TODOs in tasks.org (Scheduled)" entry (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t")
     ("m" "TODOs in tasks.org from Mail" entry (file+headline "~/TODOS/tasks.org" "Mail")
      "* TODO %? , Link: %a\nSCHEDULED: %^{SCHEDULED?}t")
     ("r" "TODOs in tasks.org Reply to" entry (file+headline "~/TODOS/tasks.org" "Mail")
      "* TODO Reply %a\nSCHEDULED: %^{SCHEDULED?}t")
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

(add-to-list 'org-latex-classes
           '("a0poster"
              "\\documentclass{a0poster}"
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; variable setting
(setenv "PATH" (concat (getenv "PATH") ":/User/haetze/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("/User/haetze/Documents/Code/lfe/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/User/haetze/.cabal/bin"))
(setq exec-path (append exec-path '("/User/haetze/.cabal/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/User/haetze/.cargo/bin"))
(setq exec-path (append exec-path '("/User/haetze/.cargo/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/Applications/Firefox.app/Contents/MacOS"))
(setq exec-path (append exec-path '("/Applications/Firefox.app/Contents/MacOS")))

(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; call function now
(set-exec-path-from-shell-PATH)

(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))
(setq latex-command "latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf  -f ")
(setq org-log-done 'time)
(setq org-archive-location "~/TODOS/archive.org::")
(setq org-agenda-start-on-weekday nil)

(setq show-paren-style 'expression)
(setq mac-option-key-is-meta nil)



(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (ditaa .t)
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

(defun compile-latex (file)
  (interactive "sfile:")
  (shell-command (concat latex-command file)))

(defun compile-latex-current-file ()
  (interactive)
  (compile-latex (buffer-file-name)))

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


(global-set-key (kbd "C-x C-g") #'git-command)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c i") #'org-insert-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x C-a") #'auto-complete-mode)
(global-set-key (kbd "C-c a") #'org-agenda)
;(global-set-key [(control ?h)] 'delete-backward-char)
(global-set-key (kbd "C-x c f") #'column-enforce-mode)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-o")
			   #'open-in-firefox-direct)))

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-x C-l")
			   #'cycle-languages)))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'erlang-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)))    
(add-hook 'lfe-mode-hook 'highlight-parentheses-mode)
(add-hook 'lfe-mode-hook (lambda ()
			   (local-set-key (kbd "C-x C-l") #'inferior-lfe)
			   (local-set-key (kbd "C-c C-c") #'compile-lfe-module)))


(elpy-enable)
;(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("-l 118"))
;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-chromium-program "open")
 '(custom-enabled-themes (quote (manoj-dark)))
 '(org-agenda-files
   (quote
    ("~/Contacts/Uni.org" "~/Contacts/Private.org" "~/TODOS/tasks.org" "~/TODOS/schedule.org")))
 '(org-capture-templates
   (quote
    (("c" "Contact Private" entry
      (file+headline "~/Contacts/Private.org" "Contacts")
      "* %^{NAME}
:PROPERTIES:
:ADDRESS: %^{EMPTY}
:BIRTHDAY: %^{BIRTHDAY}t
:EMAIL: %^{EMAIL}
:PHONE: %^{PHONE}
:NOTE: %^{NOTE}
:END:" :empty-lines 1)
     ("C" "Contact Uni" entry
      (file+headline "~/Contacts/Uni.org" "Contacts")
      "* %^{NAME}
:PROPERTIES:
:ADDRESS: %^{EMPTY}
:BIRTHDAY: %^{BIRTHDAY}t
:EMAIL: %^{EMAIL}
:PHONE: %^{PHONE}
:NOTE: %^{NOTE}
:END:" :empty-lines 1)
     ("t" "TODOs in tasks.org (Scheduled/Deadline)" entry
      (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME}
SCHEDULED: %^{SCHEDULED?}t
DEADLINE: %^{DEADLINE?}t")
     ("D" "TODOs in tasks.org (Deadline)" entry
      (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME}
DEADLINE: %^{DEADLINE?}t")
     ("S" "TODOs in tasks.org (Scheduled)" entry
      (file+headline "~/TODOS/tasks.org" "Personal")
      "* TODO %^{NAME}
SCHEDULED: %^{SCHEDULED?}t")
     ("m" "TODOs in tasks.org from Mail" entry
      (file+headline "~/TODOS/tasks.org" "Mail")
      "* TODO %? , Link: %a
SCHEDULED: %^{SCHEDULED?}t")
     ("r" "TODOs in tasks.org Reply to" entry
      (file+headline "~/TODOS/tasks.org" "Mail")
      "* TODO Reply %a
SCHEDULED: %^{SCHEDULED?}t")
     ("s" "SRCs in Code.org" entry
      (file+headline "~/TODOS/Code.org" "Code")
      "* %^{NAME} 
#+BEGIN_src %^{LANGUAGE} 
%c
#+END_src"))))
 '(org-contacts-files (quote ("~/Contacts/Private.org" "~/Contacts/Uni.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (pyenv-mode elpy py-autopep8 scala-mode lsp-mode flycheck column-enforce-mode auto-complete openwith ess-R-data-view ess use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang)))
 '(template-use-package t nil (template)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


