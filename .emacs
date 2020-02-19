(setq author "Richard Stewing")
(setq email "richard.stewing@udo.edu")
(setq user-mail-address (concat "<" author "<" email ">>"))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))


(setq package-archive-priorities
      '(("melpa" . 10)
	("org" . 5)
	("elpa" . 0)))

;; Check existence of files and copy/clone if needed
;; Dependencies:
;;               - git
;;               - curl
;;               - tar

(let ((git (locate-file "git" exec-path))
      (curl (locate-file "curl" exec-path))
      (tar (locate-file "tar" exec-path)))
  (if (and git curl tar)
      (progn
	    
	(if (not (file-directory-p "~/TODOS"))
	    (shell-command "git clone https://github.com/haetze/TODOS ~/TODOS"))
	
	(if (not (file-directory-p "~/.templates"))
	    (shell-command "git clone https://github.com/haetze/.templates ~/.templates"))

	(if (not (file-directory-p "~/.emacs.d/template"))
	    (progn
	      (shell-command "curl -L  https://sourceforge.net/projects/emacs-template/files/latest/download > template.tar.gz")
	      (shell-command "mkdir ~/.emacs.d/template")
	      (shell-command "tar -C ~/.emacs.d/template -xf template.tar.gz")
	      (shell-command "rm template.tar.gz")))

	;; copies .gnus.el in $HOME
	(if (not (file-exists-p "~/.gnus.el"))
	    (copy-file "~/usefulCommands/.gnus.el" "~/.gnus.el" t))
	
	;; create $HOME/.local/bin
	(if (not (file-directory-p "~/.local"))
	    (progn
	      (shell-command "mkdir ~/.local")
	      (shell-command "mkdir ~/.local/bin")))

	;; copies commit in $HOME/.local/bin/
	(if (not (file-exists-p "~/.local/bin/commit"))
	    (copy-file "~/usefulCommands/commit" "~/.local/bin/commit" t))

	;; copies commit in $HOME/.local/bin/
	(if (not (file-exists-p "~/.local/bin/pull"))
	    (copy-file "~/usefulCommands/commit" "~/.local/bin/pull" t)))))


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
		   'slime
		   'magit
		   'gnu-elpa-keyring-update
		   'lsp-mode
		   'lsp-ui
		   'dired-launch
		   'proof-general
		   'flyspell-popup))

(dolist (pkg my-packages)
  (install-package pkg))

(eval-when-compile
  (require 'use-package))

(defvar private-contacts
  "~/Contacts/Private.org")

(defvar work-contacts 
  "~/Contacts/Work.org")

(setq org-contacts-files
      (quote (private-contacts work-contacts)))

(defvar tasks
  "~/TODOS/tasks.org")

(defvar code-file
  "~/TODOS/Code.org")

(defvar schedule-file
  "~/TODOS/schedule.org")

(setq ispell-program-name "aspell")

;;requires and configuration
(require 'org)
(require 'org-tempo)
(require 'org-ref)
(require 'ox-extra)

(ox-extras-activate '(ignore-headlines))

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; lsp configs
(use-package lsp-mode
  :config
  ;; Prefer using lsp-ui (flycheck) over flymake.
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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
(setq vlc "/Applications/VLC.app/Contents/MacOS/VLC")
(require 'openwith)
(setq openwith-associations `(
			      ("\\.pdf\\'" "open" (file))
			      ("\\.mp4\\'" ,vlc (file))
			      ("\\.flv\\'" ,vlc (file))
			      ("\\.png\\'" "open" (file))
			      ))
(openwith-mode t)
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
  :custom (org-contacts-files `(,private-contacts
				,work-contacts)))
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

(defvar meeting-tasks
  "* MEETING %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t")

(defvar appointment-tasks
  "* APPOINTMENT %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t")

(defvar appointment-na-tasks
  "* APPOINTMENT-NO-ATTENDANCE %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t")

(defvar mail-task
  "* TODO %? , Link: %a\nSCHEDULED: %^{SCHEDULED?}t")

(defvar reply-task
  "* TODO Reply %a\nSCHEDULED: %^{SCHEDULED?}t")

;; Org-Caputre configs

;; Contact Privat
(add-to-list
 'org-capture-templates
     (list "c"
	   "Contact Private"
	   'entry
	   '(file+headline private-contacts "Contacts")
	   org-contacts-template
	   :empty-lines 1))

;; Contact Work
(add-to-list
 'org-capture-templates
 (list "C"
       "Contact Work"
       'entry '
       (file+headline work-contacts "Contacts")
       org-contacts-template
       :empty-lines 1))

;; Simple Task Scheduled and Deadline
(add-to-list
 'org-capture-templates
 (list "t"
       "TODOs in tasks.org (Scheduled/Deadline)"
       'entry
       '(file+headline tasks "Personal")
       schedule/deadline-tasks))

;; Simple Task only Deadline
(add-to-list
 'org-capture-templates
 (list "D"
       "TODOs in tasks.org (Deadline)"
       'entry
       '(file+headline tasks "Personal")
       deadline-tasks))

;; Simple Task only Scheduled
(add-to-list
 'org-capture-templates
 (list "S"
       "TODOs in tasks.org (Scheduled)"
       'entry
       '(file+headline tasks "Personal")
       schedule-tasks))

;; Meetings
(add-to-list
 'org-capture-templates
 (list "M"
       "Meeting in tasks.org"
       'entry
       '(file+headline tasks "Meetings")
       meeting-tasks))

;; Appointments
(add-to-list
 'org-capture-templates
 (list "a"
       "Appointments in tasks.org Personal"
       'entry
       '(file+headline tasks "Personal")
       appointment-tasks))

;; Appointments
(add-to-list
 'org-capture-templates
 (list "A"
       "Appointments in tasks.org (Work)"
       'entry
       '(file+headline tasks "Work")
       appointment-tasks))

;; Appointments - NO ATTENDANCE
(add-to-list
 'org-capture-templates
 (list "n"
       "Appointments (no attendance) in tasks.org (Personal)"
       'entry
       '(file+headline tasks "Personal")
       appointment-na-tasks))

;; Appointments - NO ATTENDANCE
(add-to-list
 'org-capture-templates
 (list "N"
       "Appointments (no attendance) in tasks.org (Work)"
       'entry
       '(file+headline tasks "Work")
       appointment-na-tasks))

;; Mail Task
(add-to-list
 'org-capture-templates
 (list "m"
       "TODOs in tasks.org from Mail"
       'entry
       '(file+headline tasks "Mail")
       mail-task))

;; Reply Mail Task
(add-to-list
 'org-capture-templates
 (list "r"
       "TODOs in tasks.org Reply to"
       'entry
       '(file+headline tasks "Mail")
       reply-task))

;; Safe Code Snippet
(add-to-list
 'org-capture-templates
 (list "s"
       "SRCs in Code.org"
       'entry
       '(file+headline code-file "Code")
       code-template))
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Allows accpetance of ical invites
;; The org mode export is somewhere
;; between buggy and unusable
(require 'gnus-icalendar)
(gnus-icalendar-setup)

(setq gnus-icalendar-org-capture-file "~/TODOS/tasks.org")
(setq gnus-icalendar-org-capture-headline '("Calendar")) ;;make sure to create Calendar heading first
(gnus-icalendar-org-setup)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Org archiving
;; Marking done and archive
(defun agenda-mark-done-and-archive ()
   (interactive)
   (org-agenda-todo 'done)
   (org-agenda-archive))
(define-key org-agenda-mode-map "\C-c\C-x\C-t" 'agenda-mark-done-and-archive)

(defun mark-done-and-archive ()
   (interactive)
   (org-todo 'done)
   (org-archive-subtree))
 (define-key org-mode-map "\C-c\C-x\C-t" 'mark-done-and-archive)
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



;; variable setting
;; w3m-command for web browsing
(setq w3m-command "/usr/local/bin/w3m")

(setq inferior-lisp-program "sbcl" 
      slime-contribs '(slime-fancy))

;; Local LFE install
(setenv "PATH" (concat (getenv "PATH") ":~/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("~/Documents/Code/lfe/bin")))

;; Local cabal install
(setenv "PATH" (concat (getenv "PATH") ":~/.cabal/bin"))
(setq exec-path (append exec-path '("~/.cabal/bin")))

;; Local Cargo install
(setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
(setq exec-path (append exec-path '("~/.cargo/bin")))

;; Mac Tex Path install
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

;; Set command to build pdfs
;; From Org-Mode
(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

;; From Latex
(setq latex-command "latexmk -cd -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -f ")


;; Set time to done switch
(setq org-log-done 'time)
;; Set Archive Locations
(setq org-archive-location "~/TODOS/archive.org::")
;; Set Agenda files
(setq org-agenda-files (list schedule-file
                             code-file
			     tasks))
;; Org Keywords
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
	(sequence "MEETING" "|" "HELD")
	(sequence "APPOINTMENT" "|" "COMPLETED")
	(sequence "APPOINTMENT-NO-ATTENDANCE" "|" "COMPLETED-NO-ATTENDANCE")))

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

(defun open(url &rest ignore)
  (interactive "sURL: ")
  (delete-window (shell-command (concat "open \"" url "\" &"))))

(defun open-direct ()
  (interactive)
  (w3m-print-this-url t)
  (let ((url (car kill-ring)))
    (setq kill-ring (cdr kill-ring))
    (open url)))

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
(global-set-key (kbd "C-c C-a") #'auto-complete-mode)
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
			   #'browse-url)
	    (local-set-key (kbd "C-c C-o")
			   #'open-direct)))

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-x C-l")
			   #'cycle-languages)
	    (flyspell-popup-auto-correct-mode)))


(add-hook 'erlang-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)))    
(add-hook 'lfe-mode-hook 'highlight-parentheses-mode)
(add-hook 'lfe-mode-hook (lambda ()
			   (local-set-key (kbd "C-x C-l") #'inferior-lfe)
			   (local-set-key (kbd "C-c C-c") #'compile-lfe-module)))
(add-hook 'latex-mode-hook (lambda ()
			     (local-set-key (kbd "C-c C-c") #'compile-latex-current-file)))

(add-hook 'artist-mode-hook (lambda ()
			      (setq indent-tabs-mode nil)))


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
 '(browse-url-browser-function (quote open))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (magit lsp-ui company-lsp gnu-elpa-keyring-update gherkin-mode dart-mode proof-general epresent pyenv-mode elpy py-autopep8 scala-mode lsp-mode flycheck column-enforce-mode auto-complete openwith ess-R-data-view ess use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang)))
 '(template-use-package t nil (template)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


