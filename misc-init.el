;;; misc-init --- miscellaneous utils   [sj--95/11/06]
;;;
;;; $Id: misc-init.el,v 1.12 2000/02/08 22:11:02 sj Exp $

;; Originally by Stephen Gildea, Nov. 88
;; From the LCD -- ~/as-is/swap-wins.el
(defun sj/swap-window-positions ()
  "Swap the positions of this window and the next one.
gildea Nov 88"
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
	  (other-window-hscroll (window-hscroll other-window))
	  (other-window-point (window-point other-window))
	  (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))

(defun sj/toggle-debug-on-error ()
  "Toggle the value of debug-on-error."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to %s" debug-on-error))

;; Add hackery to allow for auto-compiling of .el files on save
(defvar sj/recompile-file nil
  "A non-nil value will force a byte-recompilation on save.
This variable is buffer-local.")
(make-variable-buffer-local 'sj/recompile-file)
(defun sj/recompile-file ()
  "Byte-compile file if (buffer-local) sj/recompile-file is t.
Should be run from after-save-hook."
  (if sj/recompile-file
      (byte-compile-file (buffer-file-name))))
(when user-sj-p
  (add-hook 'after-save-hook 'sj/recompile-file))

(defun sj/replace-key-in-map (map old new)
  "Replace all occurences of command OLD in keymap MAP with command NEW."
  (mapcar
   (function
    (lambda (keyseq)
      (define-key map keyseq new)))
   (where-is-internal old map)))

(defun sj/elisp-electric-delete (&optional arg)
    "Deletes all preceding whitespace.
If however an ARG is supplied,  or point is inside a literal then
a normal delete is carried out"
    (interactive "P")
    (if arg
	;; do nothing special
	(backward-delete-char-untabify (prefix-numeric-value arg))
      (let ((here (point)))
	(skip-chars-backward " \t\n")
	(if (/= (point) here)
	    (delete-region (point) here)
	  (backward-delete-char-untabify 1)))))

(defun sj/elisp-electric-close-paren ()
  "Insert close parenthesis, go to the next line and indent."
  (interactive)
  (and (memq (preceding-char) '(?\  ?\t ?\n))
       (sj/elisp-electric-delete))
  (insert ?\))
  (newline-and-indent))

;; start a new netscape/detect existing one and pass url to it
(defvar sj/netscape-program "netscape-remote"
  "*The netscape binary.
Set this if netscape isn't on the $PATH passed to emacs.")
(setq browse-url-netscape-command sj/netscape-program)

;; Note that it is possible (but very difficult) to call sj/netscape
;; rapidly enough for this simple scheme to break.  But netscape
;; itself seems to have trouble with multiple -remote calls coming in
;; together. <*shrug*>
(defvar sj/netscape-last-url nil)

(defun sj/netscape (url &optional force-new)
  "Run netscape on URL.  If a netscape process already exists, connects to it.
If optional argument FORCE-NEW is non-nil, start a new netscape process."
  (interactive "sUrl: ")
  (setq url (sj/netscape-escape-url url))
  (or (getenv "DISPLAY")
      (error "The environment variable $DISPLAY isn't set."))
  (if force-new
      (set-process-sentinel (start-process "netscape" " *netscape*"
					   sj/netscape-program url)
			    'sj/netscape-new-sentinel)
    (setq sj/netscape-last-url url)
    (set-process-sentinel (start-process "netscape" " *netscape-remote*"
					 sj/netscape-program
					 "-remote" (format "openURL(%s)" url))
			  'sj/netscape-remote-sentinel)))

(require 'url-parse)
(defun sj/netscape-escape-url (url)
  (let ((parsed-url (url-generic-parse-url url)))
    (url-set-filename parsed-url
		      (sj/url-shell-escape (url-filename parsed-url)))
    (url-recreate-url parsed-url)))
;; stolen from w3's url.el
(defconst sj/url-escaped-chars
  '( ?$ ?! ?~ ?* ?' ?` ?\" ?\  ?\( ?\) ?, ?\& ?\# ))
(defun sj/url-shell-escape (str)
  "Escape characters in a string"
  (mapconcat
   (function
    (lambda (char)
      (if (memq char sj/url-escaped-chars)
	  (if (< char 16)
	      (upcase (format "%%0%x" char))
	    (upcase (format "%%%x" char)))
	(char-to-string char))))
   str ""))

(defun sj/netscape-new-sentinel (proc str)
  ;; sentinel for "fresh" (ie. non-remote) netscape processes
  ;; *netscape*	buffer left lying around to scan for error messages
  (if (string-match "finished" str)
      (message "Netscape terminated succesfully")
    (error "Netscape: %s" str)))

(defun sj/netscape-remote-sentinel (proc str)
  ;; sentinel for remote netscape.  checks if -remote succeeded
  ;; if it failed, starts a new netscape
  (save-excursion
    (set-buffer " *netscape-remote*")
    (unless sj/netscape-last-url  ; should never be nil here, but check anyway
      (erase-buffer)
      (error "Cannot find the url to pass to remote netscape."))
    (goto-char (point-min))
    ;; see if the -remote exec bombed; if so, start a fresh netscape.
    ;; this code assumes that netscape -remote w/o a remote netscape
    ;; returns an error message that contains the string below
    (if (search-forward "not running on display" nil t)
	(progn
	  (erase-buffer)
	  (sj/netscape sj/netscape-last-url t))
      (if (string-match "finished" str)
	  (message "Passed URL (%s) to netscape."
		   sj/netscape-last-url)
	;; if we get here the -remote bombed for some reason other than
	;; we couldn't find a remote netscape
	(error "Netscape -remote error: %s" str)))))

;; wrapper that does the right thing on tty consoles
(defun sj/browse-url-browser-function (url)
  (if (null window-system)
      (browse-url-w3 url)
    (sj/netscape url)))
;(setq browse-url-browser-function 'sj/browse-url-browser-function)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
