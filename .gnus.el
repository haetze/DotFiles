(require 'cl)
(require 'smtpmail)

(setq gnus-always-read-dribble-file t)
(setq mml-secure-openpgp-sign-with-sender t) ;; Needed so gnus looks up the uid from the sender field
;; Read HTML mail
;; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
;; shr doesn't require w3m to be installed.
;; emacs needs to be build with libxml2 support
(setq mm-text-html-renderer 'w3m)
(setq user-full-name "Richard Stewing")

(setq user-mail-address "richard.stewing@udo.edu")
(setq smtpmail-local-domain "homepc")

(setq gnus-select-method
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnir-search-engine imap)
		      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
		      (nnmail-expiry-wait 90)))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "tu-dortmund"
		      (nnimap-address "outlook.tu-dortmund.de")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnir-search-engine imap)
		      (nnmail-expiry-target "nnimap+tu-dortmund:INBOX.Trash")
		      (nnmail-expiry-wait 90)))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "stewing.dev"
		      (nnimap-address "mx2e90.netcup.net")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnir-search-engine imap)
		      (nnmail-expiry-target "nnimap+stewing.dev:INBOX.Trash")
		      (nnmail-expiry-wait 90)))


(setq gnus-posting-styles
  '((".*"
     (address "Richard Stewing <richy.sting@gmail.com>")
     ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 richy.sting")
     ("GCC" "Sent")
     (signature-file "~/DotFiles/mail-signature-gmail")
     )
    ("tu-dortmund.*"
     (address "Richard Stewing <richard.stewing@tu-dortmund.de>")
     ("X-Message-SMTP-Method" "smtp outlook.tu-dortmund.de 587 smristew")
     ("GCC" "nnimap+tu-dortmund:INBOX.Sent")
     (signature-file "~/DotFiles/mail-signature-tu-dortmund")
     )
    ("stewing.dev.*"
     (address "Richard Stewing <richard@stewing.dev>")
     ("X-Message-SMTP-Method" "smtp mx2e90.netcup.net 25 richard@stewing.dev")
     ("GCC" "nnimap+stewing.dev:Sent")
     (signature-file "~/DotFiles/mail-signature-stewing")
     )
    ))
    

(setq gnus-gcc-mark-as-read t)
(setq gnus-use-cache t)

