(require 'cl)
(require 'smtpmail)

(setq gnus-always-read-dribble-file t)
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
		      (nnimap-address "unimail.tu-dortmund.de")
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


;; Available SMTP accounts.
(defvar smtp-accounts
  '((ssl "richy.sting@gmail.com" "smtp.gmail.com" 587 "richy.sting" 'nil)
    (ssl "richy.sting@googlemail.de" "smtp.gmail.com" 587 "richy.sting" 'nil)
    (ssl "richard.stewing@udo.edu" "unimail.tu-dortmund.de" 25 "smristew" 'nil)
    (ssl "richard.stewing@tu-dortmund.de" "unimail.tu-dortmund.de" 25 "smristew" 'nil)
    (ssl "richard@stewing.dev" "mx2e90.netcup.net" 25 "richard@stewing.dev" 'nil)))

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((memq auth-mech '(cram-md5 plain login))
               (return (apply 'set-smtp (cons auth-mech auth-spec))))
              ((eql auth-mech 'ssl)
               (return (apply 'set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
          finally (error "Cannot infer SMTP information."))))

(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))
  
(ad-activate 'smtpmail-via-smtp)


(setq gnus-message-archive-group 'gnus-message-archive-group-function)
(setq gnus-gcc-mark-as-read t)

(defun gnus-message-archive-group-function (group-current)
  (cond
   ((string-match "tu-dortmund:INBOX" group-current)
    (list "nnimap+tu-dortmund:INBOX.Sent"
	  (concat "nnfolder+archive:sent." (format-time-string "%Y-%m"))))))

(setq gnus-use-cache t)
