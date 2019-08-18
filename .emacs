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
		   'racket-mode
		   'w3m))

(dolist (pkg my-packages)
  (install-package pkg))
      
;;requires and configuration
(require 'org)
(require 'org-tempo)
(require 'org-ref)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; template support
;; https://www.emacswiki.org/emacs/TemplatesMode
(add-to-list 'load-path "/home/haetze/.emacs.d/template/")
(require 'template)
(template-initialize)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Window Manager Edwina
;; https://github.com/ajgrf/edwina
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/edwina"))
(require 'edwina)

(setq edwina-keymap-prefix (kbd "C-q C-q"))

(setq edwina-mode-map
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "r") 'edwina-arrange)
    (define-key prefix-map (kbd "C-r") 'edwina-arrange)
    (define-key prefix-map (kbd "N") 'edwina-select-next-window)
    (define-key prefix-map (kbd "C-S-n") 'edwina-select-next-window)
    (define-key prefix-map (kbd "SPC") 'edwina-select-next-window)
    (define-key prefix-map (kbd "C-SPC") 'edwina-select-next-window)
    (define-key prefix-map (kbd "P") 'edwina-select-previous-window)
    (define-key prefix-map (kbd "C-S-p") 'edwina-select-previous-window)
    (define-key prefix-map (kbd "n") 'edwina-swap-next-window)
    (define-key prefix-map (kbd "C-n") 'edwina-swap-next-window)
    (define-key prefix-map (kbd "p") 'edwina-swap-previous-window)
    (define-key prefix-map (kbd "C-p") 'edwina-swap-previous-window)
    (define-key prefix-map (kbd "%") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "5") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "{") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "[") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "^") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "6") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "}") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "]") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "d") 'edwina-dec-nmaster)
    (define-key prefix-map (kbd "C-d") 'edwina-dec-nmaster)
    (define-key prefix-map (kbd "i") 'edwina-inc-nmaster)
    (define-key prefix-map (kbd "k") 'edwina-delete-window)
    (define-key prefix-map (kbd "C-k") 'edwina-delete-window)
    (define-key prefix-map (kbd "RET") 'edwina-zoom)
    (define-key prefix-map (kbd "<return>") 'edwina-zoom)
    (define-key prefix-map (kbd "c") 'edwina-clone-window)
    (define-key prefix-map (kbd "C-c") 'edwina-clone-window)
    (define-key map edwina-keymap-prefix prefix-map)
    map))


(setq edwina-mode-map-alist
  `((edwina-mode . ,edwina-mode-map)))


(edwina-mode)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Open Files with dired
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "xpdf" (file))))
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
      entry (file+headline "~/Contacts/Private.org" "Contacts"),
      org-contacts-template
      :empty-lines 1)
     ;; Contact Work
     ("C" "Contact Uni" entry (file+headline "~/Contacts/Uni.org" "Contacts"),
      org-contacts-template
      :empty-lines 1)
     ;; Simple Task Scheduled and Deadline
     ("t"
      "TODOs in tasks.org (Scheduled/Deadline)"
      entry
      (file+headline "~/TODOS/tasks.org" "Personal"),
      schedule/deadline-tasks)
     ;; Simple Task only Deadline
     ("D"
      "TODOs in tasks.org (Deadline)"
      entry
      (file+headline "~/TODOS/tasks.org" "Personal"),
      deadline-tasks)
     ;; Simple Task only Scheduled
     ("S"
      "TODOs in tasks.org (Scheduled)"
      entry
      (file+headline "~/TODOS/tasks.org" "Personal"),
      schedule-tasks)
     ;; Mail Task
     ("m"
      "TODOs in tasks.org from Mail"
      entry
      (file+headline "~/TODOS/tasks.org" "Mail"),
      mail-task)
     ;; Reply Mail Task
     ("r"
      "TODOs in tasks.org Reply to"
      entry
      (file+headline "~/TODOS/tasks.org" "Mail"),
      reply-task)
     ;; Safe Code Snippet
     ("s"
      "SRCs in Code.org"
      entry
      (file+headline "~/TODOS/Code.org" "Code"),
      code-template)
   )))
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


(setq package-enable-at-startup nil)
(package-initialize)

;; variable setting
;; Local LFE install
(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("/home/haetze/Documents/Code/lfe/bin")))

;; Local cabal install
(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/.cabal/bin"))
(setq exec-path (append exec-path '("/home/haetze/.cabal/bin")))

;; Local Cargo install
(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/.cargo/bin"))
(setq exec-path (append exec-path '("/home/haetze/.cargo/bin")))

;; Set command to build pdfs
(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

;; Set time to done switch
(setq org-log-done 'time)
;; Set Archive Locations
(setq org-archive-location "~/TODOS/archive.org::")
;; Set Schedule to start on any day 
(setq org-agenda-start-on-weekday nil)
;; Set how parens are displayed
(setq show-paren-style 'expression)



;; Set languages for the intention
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
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






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-chromium-program "firefox")
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
    (racket-mode openwith ess-R-data-view ess use-package org-plus-contrib orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang)))
 '(template-use-package t nil (template)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


