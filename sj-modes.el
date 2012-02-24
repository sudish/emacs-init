;;; sj-modes --- Initialize various modes and packages
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; Angry fruit salad
(setq font-lock-maximum-decoration t)
(require 'font-lock)

;; show-paren
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; parenface: dim the color for parens
;(require 'parenface)

(defconst sj/use-paren-mode-hooks '(emacs-lisp-mode-hook
				    lisp-mode-hook
				    lisp-interaction-mode-hook
				    clojure-mode-hook
				    slime-repl-mode-hook
				    python-mode-hook)
  "Hooks for modes where we enable special paren handling
through paredit and highlight-paren")

(defun sj/use-paren-mode-hook ()
  (paredit-mode 1)
  (setq viper-insert-local-user-map
	(sj/copy-keys-from-keymap paredit-mode-map '([?\d] [?\C-d]
						     ([?\d] . [backspace])
						     ([?\d] . [delete]))))
  (highlight-parentheses-mode t))
(dolist (hook sj/use-paren-mode-hooks)
  (add-hook hook 'sj/use-paren-mode-hook t))


;; cua-mode
(setq cua-enable-cua-keys nil
      cua-highlight-region-shift-only t
      cua-toggle-set-mark nil)
(cua-mode)

;; ibuffer -- ya improved buffer menu, included with emacs 22+
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("Clojure" (or
		     (mode . clojure-mode)
		     (name . "^\\*slime-repl clojure")))
	 ("C/C++/ObjC" (or
		   (mode . c-mode)
		   (mode . c++-mode)
		   (mode . objc-mode)
		   (mode . makefile-mode)
		   (mode . makefile-bsdmake-mode)))
	 ("Ruby"   (or
		    (mode . ruby-mode)
		    (mode . yaml-mode)
		    (mode . rhtml-mode)))
	 ("SQL" (mode . sql-mode))
	 ("Haskell" (or
		     (mode . haskell-mode)
		     (mode . literate-haskell-mode)
		     (mode . inferior-haskell-mode)
		     (name . "^\\*scion")
		     (name . "\\.cabal$")))
	 ("Python" (mode . python-mode))
	 ("Erlang" (mode . erlang-mode))
	 ("Shell"  (mode . sh-mode))
	 ("Perl"   (mode . cperl-mode))
	 ("Lisp"  (or
		   (mode . lisp-mode)
		   (name . "^\\*slime-repl")))
	 ("Emacs" (or
		   (mode . emacs-lisp-mode)
		   (name . "^\\*scratch\\*$")))
	 ("nXML"   (mode  . nxml-mode))
	 ("Git"   (or
		   (mode . magit-mode)
		   (name . "magit")))
	 ("Dired"  (mode . dired-mode))
	 ("IRC"    (mode . erc-mode))
	 ("Gnus"  (or
		   (mode . message-mode)
		   (mode . bbdb-mode)
		   (mode . mail-mode)
		   (mode . gnus-group-mode)
		   (mode . gnus-summary-mode)
		   (mode . gnus-article-mode)
		   (name . "^\\.bbdb$")
		   (name . "^\\.newsrc-dribble")))
	 ("Slime misc" (or
			(name . "^\\*inferior-lisp")
			(name . "^\\*slime"))))))
(add-hook 'ibuffer-mode-hook
	  (defun sj/ibuffer-mode-hook ()
	    (ibuffer-switch-to-saved-filter-groups "default")
	    (ibuffer-auto-mode)))

;; project-root
(require 'project-root)
(setq project-roots
      `(("Rails Project"
         :root-contains-files ("app" "config" "db" "lib" "script" "test")
         :on-hit (lambda (p) (message (car p))))
	("Emacs config"
         :path-matches ,(format "\\(%s\\)" sj/emacs-base-dir)
	 :on-hit (lambda (p) (message (car p))))
	("Perl Project"
         :root-contains-files ("t" "lib")
         :on-hit (lambda (p) (message (car p))))
	("Git Project"
         :root-contains-files (".git")
         :on-hit (lambda (p) (message (car p))))))
;; (global-set-key (kbd "C-c p f") 'project-root-find-file)
;; (global-set-key (kbd "C-c p g") 'project-root-grep)
;; (global-set-key (kbd "C-c p a") 'project-root-ack)
;; (global-set-key (kbd "C-c p d") 'project-root-goto-root)
;; (global-set-key (kbd "C-c p M-x")
;; 		'project-root-execute-extended-command)

;; ack-mode: recursive grep using ack
(sj/load-path-prepend "external/ack-mode")
(autoload 'ack "ack-mode" nil t)

;; winner: rotate through window config history
(winner-mode 1)

;; Auto-update contents of non-dirty buffers from disk
(global-auto-revert-mode 1)

;; icomplete: incremental minibuffer completion
(require 'icomplete)
(icomplete-mode 1)

;; iedit: in-place concurrent edit
(autoload 'iedit-mode "iedit" nil t)

;; complete: partial completion etc.  must be before ffap
(require 'complete)
(partial-completion-mode t)

;; pabbrev
;; (require 'pabbrev)
;; (global-pabbrev-mode)

;; diff
(setq diff-switches '("-u"))

;; vc: the other way to access cvs
;(setq vc-diff-switches '("-u" "-kk" "-N"))
(eval-when-compile
  (require 'vc))
(add-to-list 'vc-handled-backends 'Git)

;; savehist -- save minibuffer history across sessions
(savehist-mode 1)

;; sgml
(setq sgml-quick-keys t)

;; filladapt
(require 'filladapt)
(setq-default filladapt-mode t)

;; greedy-delete
(require 'greedy-delete)
(dolist (hook '(c-mode-hook haskell-mode-hook c++-mode-hook erlang-mode-hook
			    emacs-lisp-mode-hook lisp-mode-hook  ruby-mode-hook
			    clojure-mode-hook))
  (add-hook hook 'gd-add-to-mode))

;; ispell
(setq ispell-program-name "/usr/bin/ispell")

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
      ido-use-filename-at-point nil
      ido-all-frames t
      ido-use-virtual-buffers nil ; Too obtrusive, stick with anything
      ido-max-work-file-list 30
      ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))

;; recentf -- recently visited files
(setq recentf-save-file (concat user-emacs-directory ".recentf")
      recentf-max-saved-items 500)
(require 'recentf)
(recentf-mode 1)

;; comint mode
(setq-default comint-scroll-to-bottom-on-input t
	      comint-scroll-to-bottom-on-output t
	      comint-input-ignoredups t)

;; ansi-color: handle ANSI color sequences in comint mode
(add-hook 'comint-mode-hook
	  (defun sj/comint-mode-hook ()
	    (require 'ansi-color)
	    (ansi-color-for-comint-mode-on)))

;; dired
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

;; allout: better outline mode
(setq outline-mode-leaders '((c-mode . "/\\*\\*\\*_")
			     (perl-mode . "###_")))
;(load "allout" nil t)
;(outline-init t)

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

;; erc - irc client
(setq erc-server "irc.freenode.net"
      erc-nick "sudish"
      erc-autojoin-channels-alist '(("freenode.net" "#clojure"))
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
      erc-track-exclude-types '("JOIN" "PART" "NICK" "333" "353"))

;; w3 settings, from Per Abrahamsen
(setq w3-user-fonts-take-precedence t
      w3-user-colors-take-precedence t
      w3-honor-stylesheets nil
      w3-use-terminal-characters nil
      w3-horizontal-rule-char 45)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
