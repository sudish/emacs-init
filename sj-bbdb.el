;;; bbdb-init --- the Insidious Big Brother Database
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

(bbdb-initialize 'gnus 'message)

(setq bbdb/gnus-summary-prefer-real-names nil
      bbdb-message-marker-field 	'gnus-mark
      bbdb-default-area-code 		404
      bbdb-notice-auto-save-file 	t
      bbdb-offer-save 			'just-do-it
      bbdb-use-pop-up 			nil           ;window-system
      bbdb-display-layout		'one-line
      bbdb-pop-up-target-lines 		1
      bbdb-always-add-addresses 	t
      bbdb-new-nets-always-primary 	nil
      bbdb-completion-display-record 	nil)
(when user-sj-p
  (setq bbdb-user-mail-names "\\(sudish\\|sj\\)@"))

;(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus t)

(add-hook 'bbdb-change-hook 'bbdb-delete-redundant-nets)
(add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
(add-hook 'bbdb-create-hook 'bbdb-creation-date-hook)

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(when user-sj-p
  (setq bbdb-auto-notes-alist
	'(("Newsgroups"    (".*" newsgroups bbdb/gnus-update-newsgroup))
	  ("Organization"  (".*" company 0 t))))
  (setq bbdb-auto-notes-ignore
	'(("Organization" . "gateway\\|collabra server"))))
(eval-when-compile (require 'gnus-util))
(defun bbdb/gnus-update-newsgroup (groups)
  "Returns current newsgroup, if we are reading news."
  (if gnus-newsgroup-name
      (gnus-group-real-name gnus-newsgroup-name)
    ""))

;; bbdb-print: pretty print bbdb files
(eval-when-compile (require 'bbdb-print))
(autoload 'bbdb-print "bbdb-print" "Print BBDB buffer" t)
(setq bbdb-print-file-name	(expand-file-name "~/bbdb.tex")
      bbdb-print-require	'name)



;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
