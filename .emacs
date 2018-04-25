(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

(setq org-log-done 'time)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(require 'org)
(require 'org-ref)




(require 'haskell-interactive-mode)
(require 'haskell-mode)
(require 'lfe-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'lfe-mode-hook 'highlight-parentheses-mode)




(add-hook 'erlang-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'erlang-compile)
	    (global-auto-complete-mode)))

(global-auto-complete-mode)

(global-set-key (kbd "C-x C-g") #'git-command)
(global-set-key (kbd "C-x C-a") #'auto-complete-mode)
(global-set-key (kbd "C-x C-l") #'inferior-lfe)
(global-set-key (kbd "C-c a") #'org-agenda)

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
 '(browse-url-browser-function (quote browse-url-chromium))
 '(browse-url-chromium-program "chrome")
 '(org-agenda-files
   (quote
    ("~/Documents/SS2018/proseminar/ausarbeitung.org" "~/Documents/UniDortmund/Side-Porjects/Type-Theory-And-Formal-Proof.org" "~/Documents/UniDortmund/FS20162017/org-mode-Example/Example.org" "~/.emacs.d/org/agenda.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/Documents/Code/lfe/bin"))
(setq exec-path (append exec-path '("/home/haetze/Documents/Code/lfe/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/home/haetze/.cargo/bin"))
(setq exec-path (append exec-path '("/home/haetze/.cargo/bin")))


(global-set-key [(control ?h)] 'delete-backward-char)
;(gnus)
(calendar)
;;(view-diary-entries)

(setq inhibit-startup-screen t)
(setq european-calendar-style 't)


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
(global-set-key (kbd "M-s C-l") #'cycle-languages)


(defun compile-lfe-module ()
  (interactive) 
  (comint-send-string (inferior-lfe-proc) (concat "(c '" (substring buffer-file-name 0 -4) ")\n")))
(global-set-key (kbd "C-c C-c c") #'compile-lfe-module)

