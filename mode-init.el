;;; mode-init --- various modes we use   [sj--95/11/06]
;;;

;; semantic
;(setq semantic-load-turn-everything-on t)
;(require 'semantic-load)

;; jde
;(require 'jde)

;; sgml
(setq sgml-quick-keys t)

;; igrep
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload 'igrep-find "igrep" "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)
(autoload 'grep "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload 'egrep "igrep" "*Run `egrep`..." t)
(autoload 'fgrep "igrep" "*Run `fgrep`..." t)
(autoload 'agrep "igrep" "*Run `agrep`..." t)
(autoload 'grep-find "igrep" "*Run `grep` via `find`..." t)
(autoload 'egrep-find "igrep" "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep" "*Run `fgrep` via `find`..." t)
(autoload 'agrep-find "igrep" "*Run `agrep` via `find`..." t)

;; filladapt
(require 'filladapt)
(setq-default filladapt-mode t)

;; greedy-delete
(require 'greedy-delete)

;; font-locking for releng/{modules,packages}
(setq ms-releng-font-lock-keywords
      '(("#.*" . font-lock-comment-face)
	;; [<module>]
	("^\\[\\(.*\\)\\]" . (1 font-lock-keyword-face nil nil))
	;; <keyword> = <value>
	("^\\(\\w[^=]*\\)=\\(.*\\)$"
	 (1 font-lock-variable-name-face nil nil)
	 (2 font-lock-keyword-face nil nil))
	;; <whitespace><continuation line for value>
	("^[       ]+\\(.*\\)$" . (1 font-lock-keyword-face nil nil))))
(defun ms-releng-font-lock-modules ()
  ;; called when file is loaded, from the buffer that file is being read into:
  ;; set the regexps and start font-lock mode.
  (setq font-lock-defaults
	'(ms-releng-font-lock-keywords nil nil nil nil))
  (font-lock-mode t))
(defalias 'ms-releng-font-lock-packages #'ms-releng-font-lock-modules)

;; filladapt
(when nil
  (setq-default filladapt-mode t)
  (autoload 'turn-off-filladapt-mode "filladapt" nil t)
  (add-hook 'emacs-lisp-mode-hook 'turn-off-filladapt-mode)
  (add-hook 'change-log-mode-hook 'turn-off-filladapt-mode)
  )

;; ispell
;(setq ispell-program-name "c:/cygwin/usr/local/bin/ispell.exe")

;; changelog mode
(setq add-log-full-name (if user-sj-p "Sudish Joseph" (user-full-name))
      add-log-mailing-address user-mail-address)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; yasb
;(require 'yasb)

;; iswitchb
(setq iswitchb-default-method 'samewindow)
(setq-default iswitchb-method 'samewindow)
(iswitchb-default-keybindings)
(when nil
  (setq iswitchb-buffer-ignore '("Minibuf")
	iswitchb-regexp nil)
  (defvar sj/iswitchb-at-end-regexps '("^ ") ;'("^\\*" "^\\." "^ ")
    "Buffers matching these regexps are moved to the end of the buffer list.

Buffers are moved in the order of the regexps in the list.  I.e., buffers
matching later regexps will be moved further back in the list.")
  (defun sj/iswitchb-move-to-end-1 (regexp)
 ;; iswitchb-buffer-ignore is too drastic.  moving stuff to the end of
    ;; the offered list is preferable for some stuff.
    ;; `iswitchb-temp-buflist' is magical, ugh.
    (let ((hidden nil))
      (dolist (buf iswitchb-temp-buflist)
	(if (string-match regexp buf)
	    (push buf hidden)))
      (iswitchb-to-end hidden)))
  (defun sj/iswitchb-move-to-end ()
    (mapc 'sj/iswitchb-move-to-end-1 sj/iswitchb-at-end-regexps))
  (add-hook 'iswitchb-make-buflist-hook 'sj/iswitchb-move-to-end)
  )

;; efs: ange-ftp++
(setq efs-generate-anonymous-password t
      efs-default-user                "anonymous"
      ange-ftp-default-user           efs-default-user)
(add-hook 'efs-load-hook 'efs-display-ftp-activity)
(setq efs-ftp-program-name "/usr/krb5/bin/ftp"
      efs-ftp-program-args '("-i" "-g" "-v")
      efs-kerberos-private t)

(setq dired-compression-method 'gzip
      dired-dwim-target t
      dired-find-subdir t
      dired-keep-marker-copy t
      dired-mail-reader 'vm
      dired-copy-preserve-time t
      dired-backup-if-overwrite 'always
      dired-grep-zcat-program "gzcat")
(setq dired-auto-shell-command-alist ;; see dired-shell.el
      '(("\\.tar\\.\\([zZ]\\|gz\\)\\|\\.tgz$"
	 (if (memq system-type '(hpux))
	     "gzcat * | gtar xvvf -" ;; gtar's broken (install.?) on the hp's
	   "gtar xzvvf *")
	 (if (memq system-type '(hpux))
	     "gzcat * | gtar tvvf -"
	   "gtar tzvvf *"))))
(setq dired-no-confirm
      '(byte-compile
	chgrp
	chmod
	chown
	compress
	hardlink
	load
	move
	print
	shell
	symlink
	uncompress
	patch
	create-top-dir
	revert-subdirs))
(defun sj/dired-after-readin-hook ()
  (setq truncate-lines t))
(add-hook 'dired-after-readin-hook 'sj/dired-after-readin-hook)

;; escreen: window conf management
;(require 'escreen)
;(escreen-install)

;; ibuff-menu: better buffer menu
(global-set-key "\C-x\C-b" 'ibuff-menu)
(autoload 'ibuff-menu "ibuff-menu" "Edit the buffer list." t)
(setq ibuff-show-buffer-size-in-menu t)
(setq ibuff-hide-buffers-regexp
      "^\\( .*\\|\\*[^s].*\\|INBOX.*\\|\\.diary\\|\\.bbdb\\|\\.bibl\\|sent .*\\|\\*ftp .*\\)$")

;; allout: better outline mode
(setq outline-mode-leaders '((c-mode . "/\\*\\*\\*_")
			     (perl-mode . "###_")))
(load "allout" nil t)
;(outline-init t)

;; pcl-cvs
(defvar sj/cvs-orig-q-binding nil)
(add-hook 'cvs-mode-hook
	  (defun sj/cvs-mode-hook ()
	    (unless sj/cvs-orig-q-binding
	      (setq sj/cvs-orig-q-binding
		    (lookup-key (current-local-map) "q")))
	    (local-set-key "k"    'sj/cvs-kill-buffers)
	    (local-set-key "q"    'sj/cvs-quit)
	    (local-set-key [up]   'cvs-mode-previous-line)
	    (local-set-key [down] 'cvs-mode-next-line)))
(setenv "CVS_RSH" "/export/home/sj/bin/ssh-quietly")
(setenv "CVSROOT" nil 'unset)		; cvs can figure it out from CVS/
(setq cvs-diff-flags '("-u")
      cvs-program    "c:/cygwin/bin/cvs"
      cvs-cvsroot    nil
      cvs-auto-remove-handled             t
      cvs-auto-remove-handled-directories t
      cvs-changelog-full-paragraphs	  t)
(defun sj/cvs-kill-buffers ()
  (interactive)
  (loop for buf in (buffer-list)
	when (string-match "^\\*cvs-.*" (buffer-name buf))
	do (kill-buffer buf))
  (delete-other-windows))
(defun sj/cvs-quit ()
  (interactive)
  (sj/cvs-kill-buffers)
  (call-interactively sj/cvs-orig-q-binding))

;; vc: the other way to access cvs
(setq vc-diff-switches '("-u" "-kk" "-N"))
(require 'vc)

;; igrep -- a better interface to the greps
(when nil
  (require 'igrep))

;; popper
(when nil
  (when user-sj-p
    (when nil
      (add-hook 'popper-load-hook
		(defun sj/popper-load-hook ()
		  (setq popper-pop-buffers t
			popper-buffers-to-skip t
			popper-empty-min '(25))
		  ;; Define key bindings
		  (define-key global-map "\C-c1" 'popper-bury-output)
		  (define-key global-map "\C-cv" 'popper-scroll-output)
		  (define-key global-map "\C-cg" 'popper-grow-output)
		  (define-key global-map "\C-cb" 'popper-switch)
		  ;;(popper-wrap compile "*compilation*")
		  ;;(popper-wrap agrep   "*igrep*")
		  ;; Make some buffers default to 30% of the screen
		  (loop for buffer in '("^\\*igrep\\*" "^\\*compilation\\*")
			initially do (setq popper-min-heights nil)
			do (push `(,buffer 30) popper-min-heights)))))
    (setq help-selects-help-window  nil
	  window-min-height         2
	  scroll-on-clipped-lines   t
	  pixel-vertical-clip-threshold 4
	  temp-buffer-shrink-to-fit nil
	  split-window-keep-point   nil)
    (setq popper-prefix (read-kbd-macro "M-o"))
    (require 'popper)
    (popper-install))
  )

;; rfc-mode: major mode for editing RFCs
(autoload 'rfc-mode "rfc" "RFC mode" t)
(push '("rfc[0-9]*\\.txt$"           . rfc-mode) auto-mode-alist)
(push '("draft-.*-[0-9][0-9]\\.txt$" . rfc-mode) auto-mode-alist)

;; shop -- shopping lists!
(autoload 'shop "shop" nil t)
(autoload 'shop-string-list "shop")
(autoload 'shop-string "shop")
(autoload 'shop-capitalize-string "shop")
(defun sj/shop-line (&optional n)
  (let ((max-list (- (or n 79) 2))
	(shopping (concat "Shopping: "
			  (shop-capitalize-string (shop-string))))
	(must-buy (shop-string)))
    (while (< (+ (length shopping) (length must-buy)) max-list)
      (setq shopping (concat shopping ", "
			     (shop-capitalize-string must-buy)))
      (setq must-buy (shop-string)))
    shopping))

;; zenirc
(autoload 'zenirc "/home/sj/.zenirc" "Major mode to waste time" t)

;; tetris!
(autoload 'tetris "tetris" nil t)

;; comics!
(autoload 'get-comic "getcomics" nil t)
(defun get-comics-all ()
  (interactive)
  (loop for comic in '("dilbert" "robotman" "hedge" "arlonjanis")
	do (progn (get-comic comic)
		  (sit-for 5))))

;; w3 settings, from Per Abrahamsen
(setq w3-user-fonts-take-precedence t)  ; Use _my_ font.
(setq w3-user-colors-take-precedence t) ; Use _my_ colors.
(setq w3-honor-stylesheets nil)         ; No, just do it..
(setq w3-use-terminal-characters nil)   ; No weird characters.
(setq w3-horizontal-rule-char 45)       ; I said: no weird characters.



;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
