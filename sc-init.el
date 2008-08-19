;;; sc-init --- supercite    [sj 95/06/11]
;;;
;;; $Id: sc-init.el,v 1.3 1998/03/24 23:25:21 sj Exp $

(autoload 'sc-cite-original "supercite" "Supercite 3.1" t)
;(add-hook 'mail-citation-hook 'sc-cite-original)
;(add-hook 'sc-load-hook 'sc-setup-filladapt)

(setq news-reply-header-hook nil)
(setq sc-nested-citation-p         t
      sc-confirm-always-p          nil
      sc-auto-fill-region-p        nil
      sc-electric-references-p     nil
      sc-reference-tag-string	   ""
      sc-citation-leader           ""
      sc-blank-lines-after-headers 0)

(setq sc-preferred-attribution-list
      '("sc-lastchoice"
	"x-attribution"
	"bbdb-attribution"
	"initials"
	"firstname"
	"lastname"))

(setq sc-rewrite-header-list      '((sc-header-author-writes)
				    (sj/sc-header-inarticle-writes)
				    (sj/sc-header-regarding-wrote)
				    (sc-header-attributed-writes)
				    (sc-header-inarticle-writes)
				    ;(sc-header-regarding-adds)
				    ;(sc-header-on-said)
				    ;(sc-header-verbose)
				    ;(sc-no-blank-line-or-header)
				    ;(sc-no-header)
				    ))

;; seperate setups for news/mail

(setq sj/mail-vars-alist '((sc-preferred-header-style . 0)))
(setq sj/news-vars-alist '((sc-preferred-header-style . 0)))

(defun sj/setup-mail-vars ()
  "Set variables on the basis of whether its a mail or news message.
The variables in sj/mail-vars-alist are set in mail-mode buffers,
those in sj/news-vars-alist are set in other mail/news mode buffers."
  (let ((alist
	 (cond ((memq major-mode '(mail-mode message-mode)) sj/mail-vars-alist)
	       (t sj/news-vars-alist))))
    (while alist
      (let ((elem (car alist)))
	(set (car elem) (eval (cdr elem))))
      (setq alist (cdr alist)))))
(add-hook 'sc-pre-hook 'sj/setup-mail-vars)

;; custom reference lines

(defun sj/sc-header-inarticle-writes ()
  "\"In article <message-id>, <from> writes:\"
Treats \"message-id\" and \"from\" fields similar to `sc-header-on-said'."
  (let ((sc-mumble "")
	(whofrom (sc-mail-field "from")))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "In article " (sc-mail-field "message-id") ",\n")
		whofrom " writes:\n"))))

(defun sj/sc-header-regarding-wrote ()
  "\"Regarding <subject>; <from> wrote:\"
Treats \"subject\" and \"from\" fields similar to `sc-header-on-said'."
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "Regarding " (sc-mail-field "subject") "; ")
		whofrom " wrote:\n"))))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
