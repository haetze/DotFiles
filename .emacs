(setq author "Richard Stewing")
(setq email "richard.stewing@udo.edu")
(setq user-mail-address (concat "<" author "<" email ">>"))

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
		   'lfe-mode
		   'erlang
		   'org-plus-contrib 
		   'org-ref
		   'ess
		   'epresent
		   'openwith
		   'w3m
		   'dired-launch
		   'proof-general))

(dolist (pkg my-packages)
  (install-package pkg))

(eval-when-compile
  (require 'use-package))


;;requires and configuration
(require 'org)
(require 'org-tempo)
(require 'org-ref)
(require 'ox-extra)

(ox-extras-activate '(ignore-headlines))



;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(add-to-list 'load-path "~/.emacs.d/agda/")
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; template support
;; https://www.emacswiki.org/emacs/TemplatesMode
(add-to-list 'load-path "~/.emacs.d/template/")
(require 'template)
(template-initialize)

(add-to-list 
 'template-expansion-alist
 '("NAME" (insert author)))
(add-to-list 
 'template-expansion-alist
 '("EMAIL" (insert email)))
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Open Files with dired
(setq dired-launch-default-launcher '("open"))
(setf dired-launch-extensions-map
      '(("xlsx" ("libreofficedev5.3"))
	("pdf"  ("open"))
	))
(dired-launch-enable)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Configure org to use more packages for eg contacts
(use-package org
  :ensure org-plus-contrib)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Organize Contacts in Emacs
(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Contacts/Private.org"
				"~/Contacts/Uni.org")))
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<






;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Templates
(defvar org-contacts-template "* %^{NAME}
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

(defvar schedule/deadline-tasks
  "* TODO %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t\nDEADLINE: %^{DEADLINE?}t")

(defvar deadline-tasks
  "* TODO %^{NAME}\nDEADLINE: %^{DEADLINE?}t")

(defvar schedule-tasks
  "* TODO %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t")

(defvar mail-task
  "* TODO %? , Link: %a\nSCHEDULED: %^{SCHEDULED?}t")

(defvar reply-task
  "* TODO Reply %a\nSCHEDULED: %^{SCHEDULED?}t")


;; Org-Caputre configs
(use-package org-capture
  :ensure nil
  :after org
  :preface
  :custom
  (org-capture-templates
   `(
     ;; Contact Privat
     ("c"
      "Contact Private"
      entry (file+headline "~/Contacts/Private.org" "Contacts")
      ,org-contacts-template
      :empty-lines 1)
     ;; Contact Work
     ("C" "Contact Uni" entry (file+headline "~/Contacts/Uni.org" "Contacts")
      ,org-contacts-template
      :empty-lines 1)
     ;; Simple Task Scheduled and Deadline
     ("t"
      "TODOs in tasks.org (Scheduled/Deadline)"
      entry
      (file+headline "~/TODOS/tasks.org" "Personal")
      ,schedule/deadline-tasks)
     ;; Simple Task only Deadline
     ("D"
      "TODOs in tasks.org (Deadline)"
      entry
      (file+headline "~/TODOS/tasks.org" "Personal")
      ,deadline-tasks)
     ;; Simple Task only Scheduled
     ("S"
      "TODOs in tasks.org (Scheduled)"
      entry
      (file+headline "~/TODOS/tasks.org" "Personal")
      ,schedule-tasks)
     ;; Mail Task
     ("m"
      "TODOs in tasks.org from Mail"
      entry
      (file+headline "~/TODOS/tasks.org" "Mail")
      ,mail-task)
     ;; Reply Mail Task
     ("r"
      "TODOs in tasks.org Reply to"
      entry
      (file+headline "~/TODOS/tasks.org" "Mail")
      ,reply-task)
     ;; Safe Code Snippet
     ("s"
      "SRCs in Code.org"
      entry
      (file+headline "~/TODOS/Code.org" "Code")
      ,code-template)
     ))
)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Allows accpetance of ical invites
;; The org mode export is somewhere
;; between buggy and unusable
(require 'gnus-icalendar)
(gnus-icalendar-setup)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Coq setup
;; brew install opam
;; opam install coq
;; ---- Install Proof General---- Not needed using MELPA
;; git clone https://github.com/ProofGeneral/PG ~/.emacs.d/lisp/PG
;; cd ~/.emacs.d/lisp/PG
;; make
;; ---- Find Path to coqtop
;; which coqtop

;; Let Proof General find coqtop
(setq coq-prog-name "/Users/haetze/.opam/default/bin/coqtop")
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



(setq package-enable-at-startup nil)
(package-initialize)

;; variable setting
;; Local LFE install
(setenv "PATH" (concat (getenv "PATH") ":~/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("~/Documents/Code/lfe/bin")))

;; Local cabal install
(setenv "PATH" (concat (getenv "PATH") ":~/.cabal/bin"))
(setq exec-path (append exec-path '("~/.cabal/bin")))

;; Local Cargo install
(setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
(setq exec-path (append exec-path '("~/.cargo/bin")))

;; Set command to build pdfs
;; From Org-Mode
(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

;; From Latex
(setq latex-command "latexmk -cd -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -f ")

;; Set time to done switch
(setq org-log-done 'time)
;; Set Archive Locations
(setq org-archive-location "~/TODOS/archive.org::")
;; Set Schedule to start on any day 
(setq org-agenda-start-on-weekday nil)
;; Set how parens are displayed
(setq show-paren-style 'expression)
(setq mac-option-key-is-meta nil)


;; Set languages for the intention
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (ditaa .t)
			     (ruby . t)
			     (java . t)
			     (haskell . t)))

;; Set startup screen
(setq inhibit-startup-screen t)
;; Set Calendar style
(setq european-calendar-style 't)
;; Set org config
(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)


;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Local Functions
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

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun get-tex-root (file)
  (interactive "sfile: ")
  (let* ((string (get-string-from-file file))
	 (match (string-match "% *! *TeX * root *= *" string))) ;; RegExp for 'magic comments', not complete!
    (if match
	(let* ((eq-pos (string-match "=" string match))
	       (nl-pos (string-match "\n" string match)))
	  (substring string (+ 1 eq-pos) nl-pos))
      file)))

(defun compile-latex (file)
  (interactive "sfile:")
  (shell-command (concat latex-command (get-tex-root file))))

(defun compile-latex-current-file ()
  (interactive)
  (compile-latex (buffer-file-name)))

(defun open-in-firefox (url)
  (interactive "sURL: ")
  (delete-window (shell-command (concat "open \"" url "\" &"))))

(defun open-in-firefox-direct ()
  (interactive)
  (w3m-print-this-url t)
  (let ((url (car kill-ring)))
    (setq kill-ring (cdr kill-ring))
    (open-in-firefox url)))

(defun commit ()
  (interactive)
  (shell-command "commit"))

(defun pull ()
  (interactive)
  (shell-command "pull"))  
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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


;; Gloabl key bindings
(global-set-key (kbd "C-x C-g") #'git-command)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c i") #'org-insert-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x C-a") #'auto-complete-mode)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-x c f") #'column-enforce-mode)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; Local key bindings
(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-o")
			   #'open-in-firefox-direct)))

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-x C-l")
			   #'cycle-languages)))


(add-hook 'erlang-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)))    
(add-hook 'lfe-mode-hook 'highlight-parentheses-mode)
(add-hook 'lfe-mode-hook (lambda ()
			   (local-set-key (kbd "C-x C-l") #'inferior-lfe)
			   (local-set-key (kbd "C-c C-c") #'compile-lfe-module)))
(add-hook 'latex-mode-hook (lambda ()
			     (local-set-key (kbd "C-c C-c") #'compile-latex-current-file)))


;; Set up for haskell-mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-type 'stack-ghci) ; Setup to use stack for repl
;;=====DEBUG HELP FOR MACOS=====
;; If it doesn't work, it might be because the XCode License is  not
;; accepted.
;; Run `sudo xcodebuild -license' to accept license.
;; Requires root
;;=====DEBUG HELP FOR MACOS=====






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-chromium-program "open")
 '(custom-enabled-themes (quote (manoj-dark)))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (dart-mode proof-general epresent pyenv-mode elpy py-autopep8 scala-mode lsp-mode flycheck column-enforce-mode auto-complete openwith ess-R-data-view ess use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang)))
 '(template-use-package t nil (template)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


