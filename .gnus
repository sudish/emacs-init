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
      gnus-summary-thread-gathering-function #'gnus-gather-threads-by-subject
      gnus-summary-same-subject " -#-"
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-thread-hide-subtree nil
      gnus-thread-ignore-subject t
      gnus-use-cross-reference t
      gnus-summary-check-current t)

(setq nntp-connection-timeout 5)

(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number)
      gnus-thread-score-function nil)

(setq gnus-break-pages nil
      gnus-novice-user nil
      gnus-expert-user t)

(setq gnus-split-methods nil
      gnus-keep-backlog 10
      gnus-asynchronous t
      gnus-use-article-prefetch 100)

(setq gnus-article-save-directory (expand-file-name "~/News/archive/")
      gnus-default-article-saver  #'gnus-summary-save-in-mail
      gnus-prompt-before-saving   t
      gnus-use-long-file-name     t
      gnus-save-all-headers       t)

(setq gnus-ignored-from-addresses
      (regexp-opt '("sudish@gmail.com" "sudish@mindspring.com"
		    "sudish@damballa.com" "sudish@earthlink.net"
		    "sudish@yahoo.com" "sudish@absonant.org"
		    "sj@absonant.org")))

(add-hook 'gnus-suspend-gnus-hook 'gnus-group-save-newsrc)

;; show mime buttons in article buffer
(setq gnus-inhibit-mime-unbuttonizing t)

;; last choice MIME types
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; SOUP
;; (setq gnus-soup-directory (expand-file-name "~/News/Soup")
;;       gnus-soup-replies-directory (concat gnus-soup-directory "/Replies")
;;       gnus-soup-packet-directory gnus-soup-replies-directory
;;       nnsoup-tmp-directory (concat gnus-soup-directory "/tmp")
;;       nnsoup-directory (concat gnus-soup-directory "/nnsoup")
;;       nnsoup-replies-directory (concat nnsoup-directory "/replies")
;;       nnsoup-active-file (concat nnsoup-directory "/active"))

;; do total-expiry on all groups
(defun sj/gnus-do-total-expire ()
  (interactive)
  (let ((gnus-total-expirable-newsgroups "."))
    (gnus-group-expire-all-groups)))

;; posting
;(setq nnspool-inews-program "/usr/lib/news/inews"
;      nnspool-inews-switches nil)
;(setq gnus-post-method '(nnspool ""))
(setq gnus-post-method 'current)
(setq gnus-mailing-list-groups "gmane\\.*")

;; scoring
(setq gnus-kill-files-directory (expand-file-name "~/News/Killfiles/")
      gnus-summary-default-score 1
      gnus-use-adaptive-scoring nil)

(setq gnus-score-file-single-match-alist '(("." "People.SCORE")))

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
      gnus-cacheable-groups "gmane\\."
      gnus-uncacheable-groups "^nnml\\|^nnfolder\\|^nnimap")

;; Show To: header if sender is me
(setq gnus-extra-headers '(To))
(setq nnmail-extra-headers gnus-extra-headers)

;; daemons
(gnus-demon-add-handler 'sj/gnus-demon-scan-news 20 5)
(gnus-demon-init)
(defun sj/gnus-demon-scan-news ()
  (when (gnus-alive-p)
    (save-window-excursion
      (save-excursion
	(set-buffer gnus-group-buffer)
	(gnus-group-get-new-news 2)))))
(setq gnus-use-nocem nil)

;; gnus-topic
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-topic-line-format "%i%(%{%n%}%): %a %v\n")

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
(setq gnus-sorted-header-list
      '("^From:" "^Subject:" "^Date:" "^Newsgroups:" "^To:" "^Cc:"))
(setq gnus-visible-headers
      (format "^%s:"
	      (regexp-opt'("From" "Subject" "Date" "Newsgroups" "Followup-To"
			   "Reply-To" "Summary" "To" "Cc" "Posted-To"
			   "Mail-Copies-To" "Apparently-To" "Resent-From"))))
(setq gnus-boring-article-headers '(empty followup-to reply-to newgroups
					  to-address to-list cc-list))
(setq gnus-treat-hide-boring-headers 'head
      gnus-treat-unsplit-urls t
      gnus-treat-wash-html nil
      gnus-treat-date-local t
      gnus-treat-display-smileys nil
      gnus-treat-display-face nil)
;; Hide the annoying Google Groups signature cruft.
(setq gnus-signature-separator '("^--~--~---[~-]*-~-------~--~----~$")
      gnus-treat-hide-signature t)

;; fonts, colors, etc.
(setq gnus-visual t
      gnus-carpal nil)

;; use aliases while forwarding posts
(add-hook 'gnus-mail-forward-hook
	  (lambda nil
	    (setq mail-aliases t)))

;; make dabbrev look in *Article* buffers for completions
(set (make-local-variable 'dabbrev-friend-buffer-function)
     (lambda (buffer)
       (save-excursion
	 (set-buffer buffer)
	 (memq major-mode '(news-reply-mode gnus-article-mode)))))

(defun sj/gnus-position-cursor ()
  ;; replacement for gnus-goto-colon
  (beginning-of-line)
  (search-forward-regexp "[0-9]+" (point-at-eol) t))
(defun sj/gnus-startup-hook ()
  (defalias 'gnus-summary-position-point 'sj/gnus-position-cursor)
  (defalias 'gnus-group-position-point   'sj/gnus-position-cursor))
(add-hook 'gnus-startup-hook 'sj/gnus-startup-hook)


;;; Local Variables:
;;; mode:emacs-lisp
;;; sj/recompile-file:t
;;; End:
