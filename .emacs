;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________
(setq author "Richard Stewing")
(setq email "richard.stewing@udo.edu")
(setq user-mail-address (concat author "<" email ">"))

(setq ns-alternate-modifier     'none
      ns-command-modifier       'meta
      ns-function-modifier      'hyper
      ns-right-command-modifier 'super)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ;;("org" . "https://orgmode.org/elpa/")
			 ))


(setq package-archive-priorities
      '(("elpa" . 10)
	("melpa" . 9)
	("nongnu" . 8)
	;;("org" . 7)
	))


;; Menu bars
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))




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
	(shell-command "git clone git@github.com:haetze/TODOS.git ~/TODOS"))

    (if (not (file-directory-p "~/NOTES"))
	(shell-command "git clone git@github.com:haetze/NOTES.git ~/NOTES"))
	
    (if (not (file-directory-p "~/.templates"))
	(shell-command "git clone git@github.com:haetze/.templates.git ~/.templates"))

    (if (not (file-directory-p "~/DotFiles"))
	(shell-command "git clone git@github.com:haetze/.templates.git ~/DotFiles"))

    (if (not (file-directory-p "~/.emacs.d/template"))
	(progn
	  (shell-command "curl -L  https://sourceforge.net/projects/emacs-template/files/latest/download > template.tar.gz")
	  (shell-command "mkdir ~/.emacs.d/template")
	  (shell-command "tar -C ~/.emacs.d/template -xf template.tar.gz")
	  (shell-command "rm template.tar.gz")))

    ;; copies .gnus.el in $HOME
    (if (not (file-exists-p "~/.gnus.el"))
	(copy-file "~/DotFiles/.gnus.el" "~/.gnus.el" t))
	
    ;; create $HOME/.local/bin
    (if (not (file-directory-p "~/.local"))
	(progn
	  (shell-command "mkdir ~/.local")
	  (shell-command "mkdir ~/.local/bin")))))

(package-initialize)
(package-refresh-contents)
;;install packages needed
(defun install-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(setq my-packages (list
		   'ada-mode ;; requires separat compilation for parsing - which requires the GNAT compiler and gnatcoll packages
		   'use-package
		   'rust-mode
		   'git-command
		   'haskell-mode
		   'idris-mode
		   'lfe-mode
		   'erlang
		   'pdf-tools
		   'org-noter
		   'org-contrib 
		   'org-ref
		   'org-kanban
		   'org-journal
		   'ob-kotlin
		   'ess
		   'epresent
		   'openwith
		   'w3m
		   'slime
		   'kotlin-mode
		   'magit
		   'gnu-elpa-keyring-update
		   'lsp-mode
		   'lsp-ui
		   'proof-general
		   'flyspell-popup
		   'nnhackernews
		   'ace-window))

(dolist (pkg my-packages)
  (install-package pkg))

(if (not (file-exists-p "~/.emacs.d/elpa/org-contrib-0.4.2/ob-lfe.el"))
    (copy-file "~/DotFiles/ob-lfe.el" "~/.emacs.d/elpa/org-contrib-0.4.2/ob-lfe.el" t))


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

(defvar schedule-file
  "~/TODOS/schedule.org")

(defvar feeds-file
  "~/TODOS/feeds.org")


(setq ispell-program-name "aspell")

;;requires and configuration
(require 'org)
(require 'org-tempo)
(require 'org-ref)
;; (require 'ox-extra)


;; (ox-extras-activate '(ignore-headlines))

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(require 'org-journal)
(setq org-journal-dir "~/TODOS/journals")
(setq org-journal-file-type 'daily)
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-date-format "%A, %d.%m.%Y")
(setq org-journal-prefix-key "C-c j")
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
;; Ace-windwo
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))
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
;; Needs to be copied from agda install
;(add-to-list 'load-path "~/.emacs.d/elpa/agda")
;(require 'agda2)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Open Files with dired
(setq vlc "/Applications/VLC.app/Contents/MacOS/VLC")
(require 'openwith)
(setq openwith-associations `(
			      ;("\\.pdf\\'" "open" (file))
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
  :ensure org-contrib)


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

(defvar todo-link
  "* TODO %^{Job?}, Origin: %a\nSCHEDULED: %^{SCHEDULED?}t")

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
;; Job with origin
(add-to-templates-personal-and-work "l" "TODOs (Linked)" tasks-private tasks-work "Personal" "Work" todo-link)
;; Meetings
(add-to-templates-personal-and-work "m" "Meeting" tasks-private tasks-work "Meetings" "Meetings" meeting-tasks)
;; Appointments
(add-to-templates-personal-and-work "a" "Appointments" tasks-private tasks-work "Personal" "Work" appointment-tasks)
;; Appointments - NO ATTENDANCE
(add-to-templates-personal-and-work "n" "Appointments - No Attendance" tasks-private tasks-work "Personal" "Work" appointment-na-tasks)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Allows accpetance of ical invites
;; The org mode export is somewhere
;; between buggy and unusable
(require 'gnus-icalendar)
(gnus-icalendar-setup)

(setq gnus-icalendar-org-capture-file tasks-work)
(setq gnus-icalendar-org-capture-headline '("Work")) ;;make sure to create Calendar heading first
(gnus-icalendar-org-setup)

;; Calendar export
(setq org-icalendar-include-todo 'nil)
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-not-todo todo-due event-if-todo))
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Org archiving
;; Marking done and archive
(define-key org-agenda-mode-map "\C-c\C-x\C-t" 'org-agenda-archive)

(define-key org-mode-map "\C-c\C-x\C-t" 'org-archive-subtree)

;; override defaults in agenda
(define-key org-agenda-mode-map "a" 'org-agenda-archive)
(define-key org-agenda-mode-map "o" 'org-agenda-open-link)
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
(setq coq-prog-name "~/.opam/4.14.0/bin/coqtop")
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;Cedille Mode
;; (setq cedille-path "~/Documents/Code/cedille")
;; (add-to-list 'load-path cedille-path)
;; (require 'cedille-mode)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



;; variable setting
;; w3m-command for web browsing
(setq w3m-command "w3m")

(setq inferior-lisp-program "sbcl" 
      slime-contribs '(slime-fancy))


;; Local LFE install
(setenv "PATH" (concat (getenv "PATH") ":~/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("~/Documents/Code/lfe/bin")))

(defun lfe-compile ()
  (interactive)
  (let ((file (buffer-file-name)))
    (comint-send-string (inferior-lfe-proc) "(c \"")
    (comint-send-string (inferior-lfe-proc) file)
    (comint-send-string (inferior-lfe-proc) "\")\n")))


;; Local ghcup/ghc/cabal install 
(setq exec-path (append '("/Users/haetze/.ghcup/bin/") exec-path))
(setenv "PATH" (concat "/Users/haetze/.ghcup/bin:" (getenv "PATH")))

(setenv "PATH" (concat (getenv "PATH") ":/Users/haetze/.cabal/bin"))
(setq exec-path (append exec-path '("/Users/haetze/.cabal/bin")))

;; Local Cargo install
(setenv "PATH" (concat (getenv "PATH") ":/Users/haetze/.cargo/bin"))
(setq exec-path (append exec-path '("/Users/haetze/.cargo/bin")))

;; Mac Tex Path install
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

;; Lean
(setq load-path (cons ".emacs.d/lean4-mode/" load-path))

;; For ada-mode 
(setq exec-path (append exec-path '("/Users/haetze/.emacs.d/elpa/ada-mode-7.1.4/")))

;; Homebrew path
(setq exec-path (append exec-path '("/Users/haetze/Library/Python/3.9/bin/")))

;; Set command to build pdfs
;; From Org-Mode
(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f"))
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted")))

;; From Latex
(setq latex-command "latexmk -cd -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f ")


;; Set time to done switch
(setq org-log-done 'time)
;; The archiving function, does not work well with the logging 
;; Set Archive Locations
(setq org-archive-location "~/TODOS/archive.org::")
;; Set Agenda files
(setq org-feed-alist
      '(("Fefes Blog"
         "http://blog.fefe.de/rss.xml"
         "~/TODOS/feeds.org" "Fefes Blog"
	 :template "* TODO %a\n%h")
	("Richard's Blog"
	 "http://stewing.dev/html/feed.xml"
	 "~/TODOS/feeds.org" "Richard's Blog"
	 :template "* TODO %h\n%description")
	("Hacker News"
	 "https://hnrss.org/frontpage"
	 "~/TODOS/feeds.org" "Hacker News - Frontpage"
	 :template "* TODO %h %a\n%description")
	("Die Anstalt"
	 "https://www.zdf.de/rss/podcast/video/zdf/comedy/die-anstalt"
	 "~/TODOS/feeds.org" "Die Anstalt"
	 :template "* TODO %h %a")
	("Berlin direkt"
	 "https://www.zdf.de/rss/podcast/video/zdf/politik/berlin-direkt"
	 "~/TODOS/feeds.org" "Berlin Direkt"
	  :template "* TODO %h %a")
	("FreeBSD News Flash"
	 "https://www.freebsd.org/news/rss.xml"
	 "~/TODOS/feeds.org" "FreeBSD News Flash"
	 :template "* TODO %h %a")
	("FreeBSD Sec Advisory"
	 "https://www.freebsd.org/security/rss.xml"
	 "~/TODOS/feeds.org" "FreeBSD Sec Advisory"
	 :template "* TODO %h %a")
	))

(defun my/update-feeds ()
  (interactive)
  (org-feed-update-all))

  ;; (defun my/update-feeds ()
  ;;   (interactive)
  ;; (window-configuration-to-register 101)
  ;; (save-excursion
  ;;   (org-feed-update-all)
  ;;   (find-file "~/TODOS/feeds.org")
  ;;   (mark-whole-buffer)
  ;;   (call-interactively 'fill-paragraph)
  ;;   (deactivate-mark))
  ;; (if (y-or-n-p "Return to old layout?")
  ;;     (jump-to-register 101)
  ;;   (org-set-startup-visibility)))

(setq org-agenda-files (list schedule-file
              		     tasks-work
			     tasks-private
			     feeds-file))
;; Org Keywords
(setq org-todo-keywords
      '((sequence "TODO(!)" "|" "DONE(!)" "CANCELED(!)")
	(sequence "MEETING(!)" "IN-SESSION(!)" "|" "HELD(!)" "METTING-CANCELED(!)")
	(sequence "APPOINTMENT(!)" "APPOINTMENT-IN-SESSION(!)" "|" "COMPLETED(!)" "APPOINTMENT-CANCELED(!)")
	(sequence "APPOINTMENT-NO-ATTENDANCE(!)" "APPOINTMENT-NO-ATTENDANCE-IN-SESSION(!)" "|" "COMPLETED-NO-ATTENDANCE(!)" "CANCELED-NO-ATTENDANCE(!)")
	(sequence "|" "NOTE")))

;; Set Schedule to start on any day 
(setq org-agenda-start-on-weekday nil)
;; Only display todos in todo-view that are *not* scheduled
(setq org-agenda-todo-ignore-scheduled 'all)
;; Set how parens are displayed
(setq show-paren-style 'expression)
(setq mac-option-key-is-meta nil)


;; Set languages for the intention
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (ditaa . t)
			     (lisp .t)
			     (shell . t)
			     (dot . t)
			     (lfe . t)
			     (R . t)
			     (ruby . t)
			     (java . t)
			     (kotlin . t)
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

;; Finds url to comments from Hackernews RSS feed
(defun get-comments-from-string (string)
  (interactive "sstring:")
  (get-text-inbetween string "Comments URL: " "Points"))

(defun get-subject-from-string (string)
  (interactive "sstring:")
  (get-text-inbetween string "Subject: " "News"))

(defun get-text-inbetween (string start finish)
  (interactive "sstring:")
  (let* ((match-start (string-match start string))
	 (match-end (string-match finish string))) 
    (string-trim (substring string (+ match-start (length start)) (- match-end 1)))))

;;Gets string from buffer, even if not current buffer
(defun buffer-string* (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun open(url &rest ignore)
  (interactive "sURL: ")
  (window-configuration-to-register 101)
  (delete-window (shell-command (concat "open \"" url "\" &")))
  (jump-to-register 101))

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
  (window-configuration-to-register 101)
  (async-shell-command (string-join '(;; TODOS
				      "GPG_TTY=$(tty)"
				      "export GPG_TTY"
				      "cd ~/TODOS"
				      "emulate sh -c 'rm -f -- journals/*~'"
				      "git add journals/*"
				      "git commit -m \"$(date)\" -a"
				      "git push"
				      ;; Contacts
				      "cd ~/Contacts"
				      "git commit -m \"$(date)\" -a"
				      "git push"
				      ;; Notes
				      "cd ~/NOTES"
				      "emulate sh -c 'rm -f -- *~'"
				      "git add *.org"
				      "git commit -m \"$(date)\" -a"
				      "git push"
				      ) " ; ")
		       (get-buffer-create "*commit*"))
  (if (eq system-type 'darwin)
      (async-shell-command (string-join '(;; Sync to iPhone
					  ;; Only applicable on macOS
					  "cp ~/TODOS/private.org /Users/haetze/Library/Mobile\\ Documents/iCloud~com~appsonthemove~beorg/Documents/"
					  "cp ~/TODOS/schedule.org /Users/haetze/Library/Mobile\\ Documents/iCloud~com~appsonthemove~beorg/Documents/"
					  "cp ~/TODOS/work.org /Users/haetze/Library/Mobile\\ Documents/iCloud~com~appsonthemove~beorg/Documents/"
					  "cp ~/TODOS/feeds.org /Users/haetze/Library/Mobile\\ Documents/iCloud~com~appsonthemove~beorg/Documents/"
					  ) " ; ")
			   (get-buffer-create "*cp-icloud*")))

  (if (y-or-n-p "Return to old layout?")
      (jump-to-register 101)))


(defun pull ()
  (interactive)
  (window-configuration-to-register 101)
  (async-shell-command (string-join '("cd ~/TODOS"
				    "git pull"
				    "cd ~/Contacts"
				    "git pull"
				    "cd ~/NOTES"
				    "git pull"
				    )
				    " ; ")
		       (get-buffer-create "*pull*"))
  (if (y-or-n-p "Return to old layout?")
      (jump-to-register 101)))
  
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



;; Gloabl key bindings
(global-set-key (kbd "C-x C-g") #'git-command)
(global-set-key (kbd "C-h C-g") #'goto-line)
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
(global-unset-key (kbd "C-f"))


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


(add-hook 'lfe-mode-hook 'highlight-parentheses-mode)

(add-hook 'lfe-mode-hook (lambda ()
			   (local-set-key (kbd "C-x C-l") #'inferior-lfe)
			   (local-set-key (kbd "C-c C-k") #'lfe-compile)))

(add-hook 'plain-tex-mode-hook (lambda ()
			   (latex-mode)))

(add-hook 'latex-mode-hook (lambda ()
			     (local-set-key
			      (kbd "C-c C-c")
			      #'compile-latex-current-file)))

(add-hook 'artist-mode-hook (lambda ()
			      (setq indent-tabs-mode nil)))

;; Set up for haskell-mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-type 'cabal-repl) ; Setup to use stack for repl
;(setq haskell-process-type 'stack-ghci) ; Setup to use stack for repl
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
 '(browse-url-browser-function 'open)
 '(custom-enabled-themes '(manoj-dark))
 '(haskell-literate-default 'tex)
 '(ledger-reports
   '(("bal-compl" "ledger balance  -f ~/Documents/Bank/global.dat")
     ("bal-fb" "ledger balance  -f ~/Documents/Bank/FlessaBank/FB.dat")
     ("bal" "ledger [[ledger-mode-flags]] -f /Users/haetze/Documents/Bank/DeutscheBank/DB.dat bal")
     ("bal" "ledger [[ledger-mode-flags]] -f /Users/haetze/Documents/Bank/DeutscheBank/DB.dat bal")
     ("balance" "ledger -f global.dat balance")
     ("test" "ledger balance")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-contacts-files '("~/Contacts/Private.org" "~/Contacts/Work.org"))
 '(org-contacts-icon-use-gravatar nil)
 '(org-export-backends '(ascii beamer html icalendar latex))
 '(package-selected-packages
   '(templatel notmuch org-contacts zig-mode org-noter pdf-tools dockerfile-mode docker ob-go keytar lsp-grammarly lsp-haskell ob-kotlin org-present kotlin-mode nnhackernews yaml-mode org dot-mode agda2-mode org-msg ace-window ada-mode go-mode htmlize org-journal exwm org-mime org-kanban calfw calfw-org ledger-mode magit lsp-ui company-lsp gnu-elpa-keyring-update dart-mode proof-general epresent pyenv-mode elpy py-autopep8 scala-mode lsp-mode flycheck column-enforce-mode auto-complete openwith ess-R-data-view ess use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang))
 '(send-mail-function 'smtpmail-send-it)
 '(template-use-package t nil (template))
 '(warning-suppress-log-types '((auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


(put 'set-goal-column 'disabled nil)
