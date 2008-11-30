;;; mode-init --- various modes we use   [sj--95/11/06]
;;;

;; show-paren
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; cua-mode
(setq cua-enable-cua-keys nil
      cua-highlight-region-shift-only t
      cua-toggle-set-mark nil)
(cua-mode)

;; project-root
(require 'project-root)
(setq project-roots
      '(("Generic Rails Project"
         :root-contains-files ("app" "config" "db" "lib" "script" "test")
         :on-hit (lambda (p) (message (car p))))
	("Generic Perl Project"
         :root-contains-files ("t" "lib")
         :on-hit (lambda (p) (message (car p))))))

;; anything
(require 'anything)
(require 'anything-config)
(add-to-list 'anything-sources project-root-anything-config-bookmarks)
(add-to-list 'anything-sources project-root-anything-config-files)

;; gnus5
(defun sj/gnus (&optional level)
  "Wrapper to startup gnus.  Uses level 3 by default."
  (interactive "P")
  ;(require 'gnus-load)
  (gnus (or level 3)))
(defun sj/gnus-just-mail (&optional level)
  "Start gnus at level 2.  Ie., just mail groups."
  (interactive "P")
  (gnus (or level 2)))
(global-set-key "\C-cn" 'sj/gnus)
(global-set-key "\C-cN" 'sj/gnus-just-mail)

;; icomplete: incremental minibuffer completion
(require 'icomplete)
(icomplete-mode)

;; complete: partial completion etc.  must be before ffap
(require 'complete)
(partial-completion-mode t)

;; ffap: find file at point (now using ido)
(require 'ffap)
;; (ffap-bindings)
;; (require 'ffap-url)
;; (setq ffap-url-fetcher 'ffap-url-fetcher)

;; ff-paths: much-enhanced find-file
(setq ff-paths-list
  '(("\\.awk$" "$AWKPATH")              ; awk files in AWKPATH env variable.
    ("^\\." "~/")                       ; .* (dot) files in user's home
    ("\\.el$" load-path)))
(setq ff-paths-display-non-existant-filename nil
      ff-paths-use-locate nil)
(require 'ff-paths)

;; new-dabbrev: dabbrev across selectable buffers, dabbrev completion, etc.
(setq dabbrev-always-check-other-buffers t
      dabbrev-abbrev-char-regexp 	"\\sw\\|\\s_"
      dabbrev-case-fold-search 		'case-fold-search
      dabbrev-case-replace 	        'case-replace)

;; diff
(setq diff-switches '("-u"))

;; sgml
(setq sgml-quick-keys t)

;; igrep -- a better interface to the greps
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
(when nil
  (setq-default filladapt-mode t)
  (autoload 'turn-off-filladapt-mode "filladapt" nil t)
  (add-hook 'emacs-lisp-mode-hook 'turn-off-filladapt-mode)
  (add-hook 'change-log-mode-hook 'turn-off-filladapt-mode))

;; greedy-delete
(require 'greedy-delete)

;; ispell
(setq ispell-program-name "/opt/local/bin/ispell")

;; changelog mode
(setq add-log-full-name (if user-sj-p "Sudish Joseph" (user-full-name))
      add-log-mailing-address user-mail-address)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; ido -- a better iswitchb
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t
      ido-max-work-file-list 30)

;; recentf -- recently visited files
(require 'recentf)
(setq recentf-max-saved-items 100)
(defun sj/ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'sj/ido-choose-from-recentf)

;; iswitchb
;; (setq iswitchb-default-method 'always-frame)
;; (setq-default iswitchb-method 'always-frame)
;; (iswitchb-mode 1)
;; (when nil
;;   (setq iswitchb-buffer-ignore '("Minibuf")
;; 	iswitchb-regexp nil)
;;   (defvar sj/iswitchb-at-end-regexps '("^ ") ;'("^\\*" "^\\." "^ ")
;;     "Buffers matching these regexps are moved to the end of the buffer list.

;; Buffers are moved in the order of the regexps in the list.  I.e., buffers
;; matching later regexps will be moved further back in the list.")
;;   (defun sj/iswitchb-move-to-end-1 (regexp)
;;  ;; iswitchb-buffer-ignore is too drastic.  moving stuff to the end of
;;     ;; the offered list is preferable for some stuff.
;;     ;; `iswitchb-temp-buflist' is magical, ugh.
;;     (let ((hidden nil))
;;       (dolist (buf iswitchb-temp-buflist)
;; 	(if (string-match regexp buf)
;; 	    (push buf hidden)))
;;       (iswitchb-to-end hidden)))
;;   (defun sj/iswitchb-move-to-end ()
;;     (mapc 'sj/iswitchb-move-to-end-1 sj/iswitchb-at-end-regexps))
;;   (add-hook 'iswitchb-make-buflist-hook 'sj/iswitchb-move-to-end))

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
      '(byte-compile chgrp chmod chown compress hardlink load
        move print shell symlink uncompress patch create-top-dir
        revert-subdirs))
(defun sj/dired-after-readin-hook ()
  (setq truncate-lines t))
(add-hook 'dired-after-readin-hook 'sj/dired-after-readin-hook)

;; ibuff-menu: better buffer menu
(global-set-key "\C-x\C-b" 'ibuff-menu)
(autoload 'ibuff-menu "ibuff-menu" "Edit the buffer list." t)
(setq ibuff-show-buffer-size-in-menu t)
(setq ibuff-hide-buffers-regexp
      "^\\( .*\\|\\*[^s].*\\|INBOX.*\\|\\.diary\\|\\.bbdb\\|\\.bibl\\|sent .*\\|\\*ftp .*\\)$")

;; allout: better outline mode
(setq outline-mode-leaders '((c-mode . "/\\*\\*\\*_")
			     (perl-mode . "###_")))
;(load "allout" nil t)
;(outline-init t)

;; vc: the other way to access cvs
;(setq vc-diff-switches '("-u" "-kk" "-N"))
(eval-when-compile
  (require 'vc))
(add-to-list 'vc-handled-backends 'Git)

;; w3 settings, from Per Abrahamsen
(setq w3-user-fonts-take-precedence t)  ; Use _my_ font.
(setq w3-user-colors-take-precedence t) ; Use _my_ colors.
(setq w3-honor-stylesheets nil)         ; No, just do it..
(setq w3-use-terminal-characters nil)   ; No weird characters.
(setq w3-horizontal-rule-char 45)       ; I said: no weird characters.



;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
