(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;;requires
(require 'org)
(require 'org-ref)
(require 'url-util)




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
(global-set-key (kbd "C-o") #'open-in-firefox)
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

(defun open-in-firefox (url)
  (interactive "sURL: ")
  (delete-window (shell-command (concat "firefox \"" url "\" &"))))

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
 '(org-agenda-files
   (quote
    ("~/Contacts/Uni.org" "~/Contacts/Private.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (orgtbl-ascii-plot gnuplot gnuplot-mode ac-haskell-process flymake-haskell-multi org-gcal haskell-mode hasky-stack eww-lnum idris-mode flyspell-correct flyspell-correct-helm flyspell-correct-ivy flyspell-correct-popup flyspell-lazy flyspell-popup org-ref bibtex-utils highlight-parentheses w3m git-command twittering-mode swift-mode slime rustfmt rust-mode lfe-mode haskell-emacs go-complete go-autocomplete go git-commit git ghc erlang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


