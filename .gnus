;;; Gnus init file

(setq gnus-activate-foreign-newsgroups gnus-level-subscribed)

(setq gnus-select-method             '(nntp "news.gmane.org")
      gnus-secondary-select-methods  nil
      gnus-check-new-newsgroups      'ask-server
      gnus-save-killed-list          nil
      gnus-read-active-file          t
      gnus-activate-level	     3
      gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively)

(setq gnus-group-line-format "%M%S%5y %(%-45G%)[%L](%3i)%z\n"
      gnus-summary-line-format "%U%R%z%(%[%4L %-20,20f%]%) %I%s\n"
      gnus-mode-non-string-length nil)

(setq gnus-use-trees nil
      gnus-tree-minimize-window 4
      gnus-generate-tree-function 'gnus-generate-horizontal-tree)

(setq gnus-auto-select-first nil
      gnus-view-pseudos t)

(setq gnus-auto-select-next 'quietly
      gnus-auto-center-summary t
      gnus-auto-select-same nil)

(setq gnus-nov-is-evil nil
      gnus-fetch-old-headers nil
      gnus-show-threads t
      gnus-summary-make-false-root 'empty
      gnus-thread-level 4
      gnus-summary-same-subject " -#-"
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-thread-hide-subtree nil
      gnus-thread-ignore-subject t
      gnus-use-cross-reference t
      gnus-summary-check-current t)

(setq nntp-xover-is-evil nil
      nntp-connection-timeout 60)

(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number)
      gnus-thread-score-function nil)

(setq gnus-show-mime t
      gnus-break-pages t
      gnus-novice-user nil
      gnus-expert-user t)

(setq gnus-split-methods nil
;;      gnus-keep-backlog 10
      gnus-asynchronous t
      gnus-use-article-prefetch 25)

(setq gnus-article-save-directory (expand-file-name "~/News/archive/")
      gnus-default-article-saver  #'gnus-summary-save-in-mail
      gnus-prompt-before-saving   t
      gnus-use-long-file-name     t
      gnus-save-all-headers       t)

(setq gnus-ignored-from-addresses
      (concat "\\(sudish\\|sj\\)@"
	      "\\(\\(corp\\|eng\\)\\.\\)?"
	      "\\(earthlink\\|mindspring\\|yahoo\\)\\."
	      "\\(com\\|net\\)"))

(add-hook 'gnus-suspend-gnus-hook 'gnus-group-save-newsrc)

;; show mime buttons in article buffer
(setq gnus-inhibit-mime-unbuttonizing t)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; do total-expiry on all groups
(defun sj/gnus-do-total-expire ()
  (interactive)
  (let ((gnus-total-expirable-newsgroups "."))
    (gnus-group-expire-all-groups)))

;; Save gnus-killed-list in a lazy fashion.
;;
;; Rationale:
;; - A fully-populated gnus-killed-list lets one use TAB-completion
;;   more effectively, since more groups are killed than not.
;; - Setting gnus-save-killed-list to 't makes saving the newsrc file
;;   very slow, since formatting gnus-killed-list is very cpu
;;   intensive given the size of the killed list.
;;
;; So, we:
;; - Save the killed list in a separate file so that gnus-killed-list
;;   doesn't need to be formatted when .newsrc.eld is written out.
;; - Save the separate killed list file (sj/gnus-lazy-startup-file)
;;   only when we decide that gnus-killed-list has changed.
;; - Since doing an element-wise comparison of the killed list to
;;   determine whether it changed is probably going to eat the very
;;   cpu this is supposed to save, we use a heuristic to check for
;;   changes.
;;
;; The heuristic used to detect changes in gnus-killed-list is:
;; * A change in the length of the list.
;; * The first element has changed.  (Gnus mostly pushes new elements
;;   onto the head of gnus-killed-list.)
;;
;; This heuristic isn't perfect because Gnus very occasionally sorts
;; the killed list, so a session that modifies the kill list and
;; triggers a sort, yet leaves the value of the first element and the
;; number of elements unchanged will evade the heuristic.
;;

(defconst sj/gnus-lazy-startup-file (expand-file-name "~/.newsrc.lazy")
  "Gnus lazy startup file.")

(defvar sj/gnus-save-lazy-newsrc-hook nil
  "Hook run just before `sj/gnus-lazy-startup-file' is written out.")

(defvar sj/gnus-killed-list-change-parms nil
  "State used to detect changes in gnus-killed-list.
Value is either nil or a cons of form (FIRST-ELEMENT . LENGTH).")

(defun sj/gnus-read-lazy-startup-file ()
  "Read Gnus lazy startup file."
  (when (and (not gnus-save-killed-list)
	     (file-readable-p sj/gnus-lazy-startup-file))
    (condition-case nil
	(let ((coding-system-for-read gnus-startup-file-coding-system))
	  (gnus-message 5 "Reading %s..." sj/gnus-lazy-startup-file)
	  (load sj/gnus-lazy-startup-file t t t)
	  (gnus-message 5 "Reading %s...done." sj/gnus-lazy-startup-file)
	  (setq sj/gnus-killed-list-change-parms ; nil is acceptable
		(and (consp gnus-killed-list)
		     (cons (car gnus-killed-list) (length gnus-killed-list)))))
      (error
       (ding)
       (unless (gnus-yes-or-no-p
		(format "Error in %s; continue? " sj/gnus-lazy-startup-file))
	 (error "Error in %s" sj/gnus-lazy-startup-file))))))

(defun sj/gnus-save-lazy-startup-file ()
  "Save Gnus lazy startup file."
  ;; Use our heuristic to look for a change in gnus-killed-list.
  (unless (and (consp sj/gnus-killed-list-change-parms)
	       (string-equal (car gnus-killed-list)
			     (car sj/gnus-killed-list-change-parms))
	       (equal (length gnus-killed-list)
		      (cdr sj/gnus-killed-list-change-parms)))
    (save-excursion
      ;; this code copied from #'gnus-save-newsrc-file
      (set-buffer (gnus-get-buffer-create " *Gnus-lazy-newsrc*"))
      (make-local-variable 'version-control)
      (setq version-control 'never)
      (setq buffer-file-name sj/gnus-lazy-startup-file)
      (setq default-directory (file-name-directory buffer-file-name))
      (buffer-disable-undo)
      (erase-buffer)

      ;; this code copied from #'gnus-gnus-to-quick-newsrc-format
      (gnus-message 5 "Saving %s..." sj/gnus-lazy-startup-file)
      (let ((print-quoted t)
	    (print-escape-newlines t))
	(insert ";; -*- emacs-lisp -*-\n")
	(insert ";; Gnus lazy startup file.\n")

	(when (and (not gnus-save-killed-list) (consp gnus-killed-list))
	  ;; Keep the loop, we might save more stuff at a later date. -sj
	  (let* ((variables '(gnus-killed-list))
		 variable)
	    ;; Insert the variables into the file.
	    (while variables
	      (when (and (boundp (setq variable (pop variables)))
			 (symbol-value variable))
		(insert "(setq " (symbol-name variable) " '")
		(gnus-prin1 (symbol-value variable))
		(insert ")\n"))))))

      (gnus-run-hooks 'sj/gnus-save-lazy-newsrc-hook)
      (let ((coding-system-for-write gnus-startup-file-coding-system))
	(save-buffer))
      (kill-buffer (current-buffer))
      ;; the user might just have used gnus-suspend, so
      ;; sj/gnus-killed-list-change-parms should be re-initialized to
      ;; the new values to prevent re-saving of .newsrc.lazy again in
      ;; this session.
      (setq sj/gnus-killed-list-change-parms ; nil is acceptable
	    (and (consp gnus-killed-list)
		 (cons (car gnus-killed-list) (length gnus-killed-list))))
      (gnus-message 5 "Saving %s.eld...done." sj/gnus-lazy-startup-file))))

;(add-hook 'gnus-setup-news-hook  'sj/gnus-read-lazy-startup-file)
;(add-hook 'gnus-save-newsrc-hook 'sj/gnus-save-lazy-startup-file)



;; posting
;(setq nnspool-inews-program "/usr/lib/news/inews"
;      nnspool-inews-switches nil)

;(setq gnus-post-method '(nnspool ""))
(setq gnus-post-method 'current)
;(setq gnus-mailing-list-groups "^nntp\\+tabloid:internal\\.lists\\.*")

;; message archives
;(setq gnus-message-archive-method
;      `(nnfolder "archive"
;		 (nnfolder-directory "~/Mail/Outgoing/")
;		 (nnfolder-active-file "~/Mail/Outgoing/active")
;		 (nnfolder-inhibit-expiry t)
;		 (nnfolder-get-new-mail nil)))
;(setq gnus-message-archive-group
;      '((format-time-string
;	 (if (message-news-p) "%Y/news-%m-%B" "%Y/mail-%m-%B")
;	 (current-time))))

;(defun sj/gnus-mail-inbound-archive-group ()
;  "Returns current inbound mail archive group as a string."
;  (format-time-string "mail.archive.%Y-%m-%B" (current-time)))

;(defvar sj/gnus-message-variables-inherited-from-summary nil
;  "List of symbols whose buffer-local values in a message buffer are
;copied from the current summary buffer.")
;(defun sj/gnus-message-hack-summary-locals ()
;  (and (string-match "^gnus-summary" (format "%s" this-command))
;       (gnus-buffer-live-p gnus-summary-buffer)
;       (dolist (sym sj/gnus-message-variables-inherited-from-summary)
;	 (set (make-local-variable sym)
;	      (save-excursion
;		(set-buffer gnus-summary-buffer)
;		(eval sym))))))
;(add-hook 'gnus-message-setup-hook 'sj/gnus-message-hack-summary-locals)

(setq gnus-required-headers '(From Date Newsgroups Subject
				   Message-ID Lines X-Newsreader)
      gnus-required-mail-headers '(From Date To Subject Message-ID
					(optional . In-Reply-To) Lines)
      gnus-check-before-posting '(subject-cmsg multiple-headers sendsys
				  message-id from long-lines control-chars
				  size new-text redirected-followup
				  signature approved empty empty-headers))



;; scoring
(setq gnus-kill-killed nil
      gnus-kill-files-directory (expand-file-name "~/News/Killfiles/")
      gnus-summary-default-score 1
      gnus-use-adaptive-scoring nil)

(setq gnus-score-file-single-match-alist
      '(("mindspring" "MindSpring.SCORE")
	("\\.solaris\\|comp\\.sys\\.sun\\." "Solaris.SCORE")
	("emacs" "Emacs.SCORE")
	("netscape" "Netscape.SCORE")
	("chess" "Chess.SCORE")
	("games" "Games.SCORE")
	("linux" "Linux.SCORE")
	("^news" "News.SCORE")))

(add-hook 'gnus-summary-prepare-hook
 (defun sj/gnus-score-select-score-file ()
   "Set score file for group based upon gnus-score-file-single-match-alist.
The first score file in the first matching entry is used."
   (let ((score-file
	  (some #'(lambda (r)
		    (if (string-match (car r) gnus-newsgroup-name)
			(cadr r)))
		gnus-score-file-single-match-alist)))
     (if (stringp score-file)
	 (gnus-score-change-score-file score-file)))))

;; caching
(setq gnus-use-cache t
      gnus-uncacheable-groups "^nnml\\|^nnfolder\\|^nnimap")

;; nnmail
;(setq mail-sources
;      (mapcar
;       (lambda (file)
;	 (list 'file ':path (expand-file-name file)))
;       '("/var/mail/sj" "~/Mailbox" "~/Mailagent")))

;(setq nnmail-use-long-file-names nil
;      nnmail-crosspost          t
;      nnmail-procmail-suffix    ".spool"
;      nnmail-keep-last-article  t
;      nnmail-treat-duplicates   nil	; this is now useless, post-5.4
;      nnmail-message-id-cache-file (expand-file-name "~/Mail/.nnmail-cache")
;      nnmail-message-id-cache-length 1000
;      gnus-auto-expirable-newsgroups "^\\(nnml:mail\\.\\|nnimap\\).*")

;(setq mail-source-movemail-program   "movemail"
;      mail-source-delete-incoming    nil
;      mail-source-directory      (expand-file-name "~sj/Mail/Incoming"))

;(setq nnmail-split-methods (function nnmail-split-fancy))
;(when user-sj-p
;  (load "~/.nnmail-split"))
(add-hook 'nnmail-prepare-incoming-header-hook 'nnmail-remove-list-identifiers)
;(setq nnmail-list-identifiers
;      '("(agent-users [0-9]+)"
;	"\\[SUPERCITE\\]"
;	"\\[GOODART\\]"
;	"\\[WM\\]"
;	"NNTPC: "
;	"\\[Pilot-Unix\\]"
;	"\\[SQL\\]"
;	"\\[sawmill\\]"
;	"\\[wm-user\\]"
;	"\\[wm-dev\\]"
	"\\[GENERAL\\]"
;	"\\[ADMIN\\]"
;	"\\[HACKERS\\]"
;	"nntp-extensions"
;	"(IPng [0-9]+)"
;	"ietf-nntp"))
;; Show To: header if sender is me
(setq gnus-extra-headers '(To))
(setq nnmail-extra-headers gnus-extra-headers)

;; control expiry
;(defun sj/nnmail-expiry-wait-function (group)
;  (cond ((string-match "personal" group) 31)
;	((string-match "mindspring" group) 31)
;	(t 14)))
;(setq nnmail-expiry-wait-function #'sj/nnmail-expiry-wait-function)

;; nnml
;(setq nnml-directory       (expand-file-name "~/Mail/")
;      nnml-active-file     (concat nnml-directory ".active")
;      nnml-newsgroups-file (concat nnml-directory ".newsgroups")
;      nnml-get-new-mail    t
;      nnml-nov-is-evil     nil)

;; NoCem
;(setq gnus-use-nocem nil
;      gnus-nocem-groups '("news.lists.filters"
;			  "news.admin.net-abuse.bulletins"
;			  "news.admin.net-abuse.announce"))

;; daemons
(setq gnus-use-demon t)
(gnus-demon-add-handler 'sj/gnus-demon-scan-news 20 5)
;(gnus-demon-add-handler 'gnus-demon-scan-nocem   30 10)
(gnus-demon-init)
(defun sj/gnus-demon-scan-news ()
  (when (gnus-alive-p)
    (save-window-excursion
      (save-excursion
	(set-buffer gnus-group-buffer)
	(gnus-group-get-new-news 2)))))
(setq gnus-use-nocem nil)

;; grouplens
;(setq gnus-use-grouplens nil
;      grouplens-pseudonym "atreides"
;      grouplens-bbb-host "grouplens.cs.umn.edu"
;      gnus-summary-grouplens-line-format "%U%R%z%l%(%[%4L %-20,20a%]%) %I%s\n"
;      grouplens-prediction-display 'prediction-num
;      gnus-grouplens-override-scoring 'combine)

;; gnus-topic
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-group-topic-topics-only t
      gnus-topic-unique t
      gnus-topic-hide-subtopics nil
      gnus-topic-line-format "%i%(%{%n%}%): %a %v\n")

;; gnus-uu
(setq gnus-uu-view-and-save t)
(setq gnus-uu-do-not-unpack-archives t)

;; window config
(gnus-add-configuration
 '(frame 1.0
	 (horizontal ((height . 60) (width . 139))
		     (vertical 80
			       (article 1.0))
		     (vertical 1.0
			       (group 0.4)
			       (summary 1.0)))))

;; headers
(defconst gnus-sorted-header-list
  '("^From:" "^Subject:" "^Date:" "^Summary:" "^Keywords:"
    "^Newsgroups:" "^To:" "^Cc:"  "^Organization:"))
(defconst gnus-visible-headers
  '("^From:" "^Subject:" "^Date:" "^Newsgroups:" "^Followup-To:" "^Reply-To:"
    "^Summary:" "^To:" "^Cc:" "^Posted-To:" "^Mail-Copies-To:"
    "^Apparently-To:" "^Resent-From:"))

;; fonts, colors, etc.
(setq gnus-visual 	   t
      gnus-carpal 	   nil
      gnus-button-url      (function sj/netscape))

;; picons
;(setq gnus-use-picons t
;      gnus-picons-database "/usr/lib/picons"
;      gnus-picons-display-where 'article)
;(add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;(add-hook 'gnus-summary-prepare-hook 'gnus-group-display-picons t)
;(add-hook 'gnus-article-display-hook 'gnus-picons-article-display-x-face)
;; from a post by hniksic
(if (featurep 'xface)
    (set-face-background 'gnus-x-face (face-background 'default)))

;; smilies
(setq gnus-treat-display-smileys nil)

;; use aliases while forwarding posts
(add-hook 'gnus-mail-forward-hook
	  (lambda nil
	    (setq mail-aliases t)))

;; for use as %uB in gnus-summary-line-format
(defvar bbdb/gnus-known-mark "+"
  "String used to mark known posters (see `bbdb/gnus-mark-known-posters').

If you wish to have this mark depend upon the identity of the poster, see
the documentation for bbdb-message-marker-field.  If such a field exists,
its value will override bbdb/gnus-known-mark.")

(defun gnus-user-format-function-Z (header)
  "Given a GNUS message header returns the BBDB name of the sender.
Should be used by including %uB in gnus-summary-line-format.

Note that bbdb/gnus-lines-and-from-length is now obsolete, and is ignored.
Better formatting can be achieved through gnus-summary-line-format."
  (let* ((from (mail-header-from header))
	 (data (and (or bbdb/gnus-mark-known-posters
			bbdb/gnus-header-show-bbdb-names)
		    (condition-case ()
			(mail-extract-address-components from)
		      (error nil))))
	 (name (car data))
	 (net (car (cdr data)))
	 (record (and data
		      (bbdb-search-simple name
		       (if (and net bbdb-canonicalize-net-hook)
			   (bbdb-canonicalize-address net)
			 net))))
	 )

    (if (and record name (member (downcase name) (bbdb-record-net record)))
	;; bogon!
	(setq record nil))

    (setq name (or (and bbdb/gnus-header-prefer-real-names
			(or (and bbdb/gnus-header-show-bbdb-names record
				 (bbdb-record-name record))
			    name))
		   net))
    (format "%s%s"
	    (if (and record bbdb/gnus-mark-known-posters)
		(or (bbdb-record-getprop
		     record bbdb-message-marker-field)
		    bbdb/gnus-known-mark)
	      " ")
	    (or name from))))


;; This makes the new dabbrev package look in *Article* buffers for completions
(set (make-local-variable 'dabbrev-friend-buffer-function)
     (lambda (buffer)
       (save-excursion
	 (set-buffer buffer)
	 (memq major-mode '(news-reply-mode gnus-article-mode)))))

;; posted by Mats Löfdahl to gnu.emacs.gnus, Apr. 20 1997
(add-hook 'gnus-ps-print-hook
 (defun ml/gnus-ps-print-hook ()
   (setq ps-left-header
	 (list (concat "(" (mail-header-subject gnus-current-headers) ")")
	       (concat "(" (mail-header-from gnus-current-headers) ")")))
   (setq ps-right-header
	 (list "/pagenumberstring load"
	       (concat "(" (mail-header-date gnus-current-headers) ")")))))

(add-hook 'gnus-startup-hook
 (defun sj/gnus-startup-hook ()
   (defalias 'gnus-summary-position-point 'sj/gnus-position-cursor)
   (defalias 'gnus-group-position-point   'sj/gnus-position-cursor)))
(defun sj/gnus-position-cursor ()
  ;; replacement for gnus-goto-colon
  (beginning-of-line)
  (search-forward-regexp "[0-9]+" (point-at-eol) t))


;;; Local Variables:
;;; mode:emacs-lisp
;;; sj/recompile-file:t
;;; End:
