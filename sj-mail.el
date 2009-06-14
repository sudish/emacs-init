;;; sj-mail --- Mail-related settings
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; packages used to customise mail
(require 'mail-hist)
(mail-hist-enable)

(eval-when-compile
  (require 'message))

;; smtpmail
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'message-smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" ""))
      starttls-use-gnutls t
      smtpmail-debug-info nil)

(setq display-time-mail-file 'dont-check)
(setq message-cite-function 'message-cite-original-without-signature
      message-post-method '(nnspool "")
      message-generate-headers-first nil
      message-signature-file "~/.gnus-signature"
      message-mailer-swallows-blank-line nil
      message-send-mail-function #'smtpmail-send-it)
(setq rmail-dont-reply-to-names "sudish\\|sj")

(autoload 'footnote-mode "footnote" nil t)
(add-hook 'message-setup-hook
  (defun sj/message-setup-hook ()
    ;;(push '(sender . disabled) message-syntax-checks)
    (footnote-mode)
    (turn-on-font-lock)))

;; speeling and it's grammaire
(defun sj/check-grammar ()
  (interactive)
  (require 'ispell)
  (save-excursion
    (or (interactive-p)
	(message-goto-body))
    (while (re-search-forward "\\<it\\('?s\\)\\>" nil t)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (unless (y-or-n-p "Is that correct usage? ")
	(let ((suffix (match-string 1))
	      (case-replace t))
	  (goto-char (match-beginning 0))
	  (cond ((string-equal suffix "'s")
		 (forward-char 2)
		 (delete-char 1))
		(t
		 (forward-char 2)
		 (insert ?')))))
      (isearch-dehighlight))))
;(add-hook 'message-send-hook 'sj/check-grammar)
(add-hook 'message-send-hook 'ispell-message)

;; additions to the mail mode keymap
(add-hook 'message-mode-hook 'sj/mail-mode-define-keys)
(add-hook 'mail-mode-hook 'sj/mail-mode-define-keys)
(add-hook 'vm-mail-mode-hook 'sj/mail-mode-define-keys)

(defun sj/mail-mode-define-keys ()
  ;;(local-set-key "\C-n" 'mail-abbrev-next-line)
  ;;(local-set-key [down] 'mail-abbrev-next-line)
  ;;(local-set-key "\M->" 'mail-abbrev-end-of-buffer)
  (local-set-key "\C-c\C-ff" (lookup-key (current-local-map) "\C-c\C-f\C-f"))
  (local-set-key "\C-c\C-f\C-f" 'sj/mail-set-from)
  (local-set-key "\C-c\C-f\C-r" 'sj/mail-return-receipt-to))

;; Make it easy to override both the envelope and header from addresses.
(defvar sj/mail-from-list '("Sudish Joseph <sudish@gmail.com>"
			    "Sudish Joseph <sudish@absonant.org>"
			    "Sudish Joseph <sudish@mindspring.com>")
  "List of alternative From: headers.")

(defvar sj/mail-from-history sj/mail-from-list ; prime the history
  "History list for sj/mail-set-from.")

(defun sj/mail-set-from ()
  (interactive)
  (let* ((from
	  (completing-read "From address: "
			   (mapcar #'(lambda (x) 
				       (cons x nil)) sj/mail-from-list)
			   nil
			   nil
			   (cons (car sj/mail-from-list) 0)
			   'sj/mail-from-history))
	 (return-path
	  (when (message-mail-p)
	    (require 'mail-extr)
	    (car (cdr-safe (mail-extract-address-components from))))))
    (cond
     ((eql 0 (length from))
      (error "Empty address"))
     ((and (message-mail-p)
	   (null return-path))
      (error "Couldn't parse address: %s" from))
     (t
      (save-excursion
	(save-restriction
	  (message-narrow-to-headers)
	  (message-remove-header "From")
	  (message-remove-header "Return-Path")
	  ;; this is only useful if you use qmail, where return-path
	  ;; sets the envelope from address
	  (if (message-mail-p)
	      (message-add-header (format "Return-Path: <%s>" return-path)))
	  (message-add-header (format "From: %s" from))))))))

;; Rewrite outgoing headers to use preferred form for certain addresses.
(defvar sj/gnus-address-rewrite-alist
  '(("april@\\(corp\\.earthlink\\|mindspring\\)\\.net"
     . "april@mindspring.com")
    ("\\(anne\\.\\)?peavler@\\(corp\\.earthlink\\|mindspring\\)\\.net"
     . "peavler@mindspring.com")
    ("ahobson@mindspring\\.net" . "ahobson@mindspring.com")
    ("mflanagan@\\(corp\\.earthlink\\|mindspring\\)\\.net"
     . "mflanagan@mindspring.com"))
  "Alist of form (REGEXP . REPLACEMENT).
Any matches for REGEXP in To:, CC:, or Bcc: headers are prompted for
replacement with REPLACEMENT.")
(defun sj/gnus-rewrite-outgoing-addresses ()
  "Applies rewrites specified in sj/gnus-address-rewrite-alist to current message."
  (interactive)
  (save-excursion
    (let ((header-regexp "^\\(resent-\\)?\\(to\\|cc\\|bcc\\):")
	  (end-of-headers (prog2
			      (message-narrow-to-headers)
			      (point-max-marker)
			    (widen))))
      (goto-char (point-min))
      (while (re-search-forward header-regexp end-of-headers t)
	;; found a matching header: apply all possible replacements to it
	(loop for (old . new) in sj/gnus-address-rewrite-alist do
	  (beginning-of-line)
	  (when (re-search-forward old nil t)
	    (isearch-highlight (match-beginning 0) (match-end 0))
	    (when (y-or-n-p
		   (format "Replace %s with %s? " (match-string 0) new))
	      (replace-match new 'fixed-case 'literally)
	      (undo-boundary))
	    (isearch-dehighlight)))
	;; we're done with this header line; don't infloop over it
	(end-of-line)))))
;;(add-hook 'message-send-hook 'sj/gnus-rewrite-outgoing-addresses)

(defun sj/mail-return-receipt-to ()
  "*Create Return-Receipt-To header."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Return-Receipt-To" t)
	(progn
	  (if (mail-position-on-field "from" t)
	      (mail-position-on-field "from")
	    (mail-position-on-field "To"))
	  (insert "\nReturn-Receipt-To: "
		  (or user-mail-address
		      (concat (user-login-name) "@" (system-name))))))))

;; FIXME: complete this hack someday
(defun sj/mail-insert-special-header (header value &optinal overwrite)
  "Insert header HEADER with value VALUE into current message.
If OVERWRITE is non-nil, any existing header is overwritten."
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "X-No-Archive" t)
	(progn
	  (if (mail-position-on-field "from" t)
	      (mail-position-on-field "from")
	    (mail-position-on-field "To"))
	  (insert "\nReturn-Receipt-To: "
		  (or user-mail-address
		      (concat (user-login-name) "@" (system-name))))))))

;; reporter: generic bug report package
(setq reporter-mailer '(message-mail reporter-mail))

(provide 'mail-init)



;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
