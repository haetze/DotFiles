(setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups


(add-to-list 'gnus-secondary-select-methods
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
		      (nnmail-expiry-target "nnimap+tu-dortmund:[Gmail]/Trash")
		      (nnmail-expiry-wait 90)))


;; Setup to send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "postserver.tu-dortmund.de"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")

