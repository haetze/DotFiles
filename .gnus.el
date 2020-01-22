;;(setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups
(setq gnus-always-read-dribble-file t)

(setq gnus-select-method
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnir-search-engine imap)
		      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
		      (nnmail-expiry-wait 90)))


(setq user-full-name "Richard Stewing"
      user-mail-address "richard.stewing@udo.edu")

;; Read HTML mail
;; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
(setq mm-text-html-renderer 'w3m)

;; Setup to send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")



(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "tu-dortmund"
		      (nnimap-address "unimail.tu-dortmund.de")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnir-search-engine imap)
		      (nnmail-expiry-target "nnimap+tu-dortmund:INBOX.Trash")
		      (nnmail-expiry-wait 90)))


;; Setup to send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "unimail.tu-dortmund.de"
      smtpmail-smtp-service 25
      smtpmail-local-domain "homepc")



(setq gnus-message-archive-group 'gnus-message-archive-group-function)
(setq gnus-gcc-mark-as-read t)

(defun gnus-message-archive-group-function (group-current)
  (cond
   ((string-match "tu-dortmund:INBOX" group-current)
    (list "nnimap+tu-dortmund:INBOX.Sent"
	  (concat "nnfolder+archive:sent." (format-time-string "%Y-%m"))))))

