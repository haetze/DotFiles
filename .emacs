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

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;Mode Line definition
;;Command to get the battery status
;;
(defvar bat-command "")
(setq bat-command (let ((uname (substring (shell-command-to-string "uname") 0 -1)))
		    (cond
		     ((string= uname "Linux")
		      "cat /sys/class/power_supply/BAT0/capacity")
		     ((string= uname "Darwin")
		      "pmset -g batt | tail -1 | awk '{print substr($3,0,2)}'")
		     (t ""))))

(setq-default mode-line-format
	      (list "%e"
		    'mode-line-front-space
		    'mode-line-mule-info
		    'mode-line-client
		    'mode-line-modified
		    'mode-line-remote
		    'mode-line-frame-identification 
		    'mode-line-buffer-identification
		    'mode-line-modes
		    '(which-func-mode ("" which-func-format "--"))
		    'mode-line-position
		    `(vc-mode vc-mode)
		    " "
		    '((:eval (concat (substring (shell-command-to-string bat-command) 0 -1) "%%")))
		    )
	      )
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


;; Check existence of files and copy/clone if needed
;; Dependencies:
;;               - git
;;               - curl
;;               - tar

(defvar git-package "git")
(defvar curl-package "curl")
(defvar tar-package "tar")

(defun install-os (pkg)
  (shell-command (concat "brew install" pkg)))

(let ((git (locate-file "git" exec-path))
      (curl (locate-file "curl" exec-path))
      (tar (locate-file "tar" exec-path)))

  (if (not git)
      (install-os "git"))
  (if (not curl)
      (install-os "curl"))
  (if (not tar)
      (install-os "tar"))
  
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
	(copy-file "~/usefulCommands/commit" "~/.local/bin/pull" t))))


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
		   'org-kanban
		   'org-journal
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
		   'flyspell-popup
		   'calfw))

(dolist (pkg my-packages)
  (install-package pkg))

(eval-when-compile
  (require 'use-package))

(defvar private-contacts
  "~/Contacts/Private.org")

(defvar work-contacts 
  "~/Contacts/Work.org")

(defvar tasks-private
  "~/TODOS/private.org")

(defvar tasks-work
  "~/TODOS/work.org")

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
(require 'org-journal)
(setq org-journal-dir "~/TODOS/journals")
(setq org-journal-file-type 'daily)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Using ``cfw:open-org-calendar'' creates a calednar view for the agenda
(require 'calfw-org)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; (install-package 'exwm) ;; if not installed
;; Uncomment if emacs is used as a wm
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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
;; Rust
(setq lsp-rust-server 'rust-analyzer)
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
			      ("\\.jpg\\'" "open" (file))
			      ("\\.PNG\\'" "open" (file))
			      ("\\.JPG\\'" "open" (file))
			      ))
(openwith-mode t)
(add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
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
  :custom
  (org-contacts-files `(,private-contacts
			,work-contacts))
  (org-contacts-icon-use-gravatar nil))
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

(defvar note
  "* NOTE %^{NAME}\nSCHEDULED: %^{SCHEDULED?}t")


(defvar mail-task
  "* TODO %^{Todo?}, Link: %a\nSCHEDULED: %^{SCHEDULED?}t")

(defvar reply-task
  "* TODO Reply %a\nSCHEDULED: %^{SCHEDULED?}t")

;; Org-Caputre configs

(defun add-to-templates-personal-and-work (key text file-p file-w headline-p headline-w template)
  (add-to-list
   'org-capture-templates
   (list key
	 (concat text " (Personal)")
	 'entry
	 `(file+headline ,file-p ,headline-p)
	 template))

  (add-to-list
   'org-capture-templates
   (list (upcase key)
	 (concat text " (Work)")
	 'entry
	 `(file+headline ,file-w ,headline-w)
	 template)))
  

;; Contacts
(add-to-templates-personal-and-work "c" "New Contact" private-contacts work-contacts "Contacts" "Contacts" org-contacts-template)
;; Simple Task Scheduled and Deadline
(add-to-templates-personal-and-work "t" "TODOs (Scheduled/Deadline)" tasks-private tasks-work "Personal" "Work" schedule/deadline-tasks)
;; Simple Task only Deadline
(add-to-templates-personal-and-work "d" "TODOs (Deadline)" tasks-private tasks-work "Personal" "Work" deadline-tasks)
;; Simple Task only Scheduled
(add-to-templates-personal-and-work "s" "TODOs (Scheduled)" tasks-private tasks-work "Personal" "Work" schedule-tasks)
;; Meetings
(add-to-templates-personal-and-work "k" "Meeting" tasks-private tasks-work"Meetings" "Meetings" meeting-tasks)
;; Appointments
(add-to-templates-personal-and-work "a" "Appointments" tasks-private tasks-work "Personal" "Work" appointment-tasks)
;; Appointments - NO ATTENDANCE
(add-to-templates-personal-and-work "n" "Appointments - No Attendance" tasks-private tasks-work "Personal" "Work" appointment-na-tasks)
;; Note
(add-to-templates-personal-and-work "l" "Note" tasks-private tasks-work "Personal" "Work" note)
;; Mail Task
(add-to-templates-personal-and-work "m" "TODOs from Mail" tasks-private tasks-work "Mail" "Mail" mail-task)
;; Reply Mail Task
(add-to-templates-personal-and-work "r" "Reply-To" tasks-private tasks-work "Mail" "Mail" reply-task)

;; Safe Code Snippet
(add-to-list
 'org-capture-templates
 (list "+"
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
(define-key org-agenda-mode-map "\C-c\C-x\C-t" 'org-agenda-archive)

(define-key org-mode-map "\C-c\C-x\C-t" 'org-archive-subtree)
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
(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f"))

;; From Latex
(setq latex-command "latexmk -cd -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f ")


;; Set time to done switch
(setq org-log-done 'time)
;; The archiving function, does not work well with the logging 
;; Set Archive Locations
(setq org-archive-location "~/TODOS/archive.org::")
;; Set Agenda files
(setq org-agenda-files (list schedule-file
                             code-file
			     tasks-work
			     tasks-private))
;; Org Keywords
(setq org-todo-keywords
      '((sequence "TODO(!)" "|" "DONE(!)")
	(sequence "MEETING(!)" "IN-SESSION(!)" "|" "HELD(!)")
	(sequence "APPOINTMENT(!)" "APPOINTMENT-IN-SESSION(!)" "|" "COMPLETED(!)")
	(sequence "APPOINTMENT-NO-ATTENDANCE(!)" "APPOINTMENT-NO-ATTENDANCE-IN-SESSION(!)" "|" "COMPLETED-NO-ATTENDANCE(!)")
	(sequence "|" "NOTE")))

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

(defun google (search &rest ignore)
  (interactive "sSearch Term: ")
  (let ((base "http://www.google.com/search?q=")
	(search-space-replace (replace-regexp-in-string "[\s\t]+" "+" search)))
    (open (concat base search-space-replace " &"))))

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
(global-set-key (kbd "C-c d") #'cfw:open-org-calendar)
(global-set-key (kbd "C-c C-a") #'auto-complete-mode)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-x c f") #'column-enforce-mode)
(global-set-key (kbd "<left>") 'shrink-window-horizontally)
(global-set-key (kbd "<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<down>") 'shrink-window)
(global-set-key (kbd "<up>") 'enlarge-window)
(global-set-key (kbd "C-c m") #'compile)
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-m"))

;; Movement Rebind
(global-set-key (kbd "C-l") #'forward-char)
(global-set-key (kbd "M-l") #'forward-word)

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
(add-hook 'plain-tex-mode-hook (lambda ()
			   (latex-mode)))
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
 '(ledger-reports
   (quote
    (("balance" "ledger -f global.dat balance")
     ("test" "ledger balance")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(org-contacts-files (quote ("~/Contacts/Private.org" "~/Contacts/Work.org")))
 '(org-contacts-icon-use-gravatar nil)
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (org-journal exwm org-mime org-kanban calfw calfw-org ledger-mode magit lsp-ui company-lsp gnu-elpa-keyring-update gherkin-mode dart-mode proof-general epresent pyenv-mode elpy py-autopep8 scala-mode lsp-mode flycheck column-enforce-mode auto-complete openwith ess-R-data-view ess use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang)))
 '(template-use-package t nil (template)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


