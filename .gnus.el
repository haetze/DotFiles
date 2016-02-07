(require 'nnir)

;; @see http://www.emacswiki.org/emacs/GnusGmail#toc1
(setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups

;; ask encyption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; @see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnir-search-engine imap)
					; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
		      ;; press 'E' to expire email
		      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
		      (nnmail-expiry-wait 90)))


(setq user-full-name "Richard Stewing"
      user-mail-address "richy.sting@gmail.com")

;; Read HTML mail
;; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
(setq mm-text-html-renderer 'w3m)

;; Setup to send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")

