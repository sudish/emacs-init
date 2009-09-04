;;; sj-langs --- Programming language modes
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; SQL mode
(eval-when-compile (require 'sql))
(setq sql-product 'postgres)

;; eldoc: automatic docs in minibuffer
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Lisp HyperSpec from http://www.lispworks.com/documentation/common-lisp.html
(setq sj/hyperspec-dir (expand-file-name
			"~/src/CommonLisp/HyperSpec-7.0/HyperSpec/")
      common-lisp-hyperspec-root (concat "file://" sj/hyperspec-dir)
      common-lisp-hyperspec-symbol-table (concat sj/hyperspec-dir
						 "Data/Map_Sym.txt"))

;; Clojure mode
(sj/load-path-prepend '("external/clojure-mode"))
(setq clojure-mode-use-backtracking-indent t
      clojure-mode-font-lock-comment-sexp t)
(autoload 'clojure-mode "clojure-mode" "A mode for clojure lisp" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'viper-vi-state-mode-list 'clojure-mode)
(autoload 'clojure-indent-function "clojure-mode") ; for use in slime buffers
(eval-when-compile (require 'parenface))
(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook
	     (paren-face-add-support clojure-font-lock-keywords)))

;; Swank-clojure for Slime integration
(sj/load-path-prepend '("external/swank-clojure"))
(setq swank-clojure-compile-p t)
(let ((path (file-name-directory (file-truename
				  (locate-library "swank-clojure")))))
  ;; Use a wrapper shell script to start clojure.  The swank-clojure
  ;; swank/ directory must be on the classpath for SLIME to run.
  (setq swank-clojure-binary (list "~/bin/clojure" "-C" path)))
(require 'swank-clojure-autoload)

;; SBCL
(defvar sj/slime-sbcl-path "/opt/local/bin/sbcl")
(eval-after-load 'slime
  '(add-to-list 'slime-lisp-implementations
		`(sbcl (,sj/slime-sbcl-path) :coding-system utf-8-unix) t))

;; Slime -- Superior Lisp Interaction Mode
(sj/load-path-prepend "external/slime" "doc")
(require 'slime-autoloads)
(eval-after-load 'slime
  '(progn
     (setq slime-default-lisp 'clojure
	   slime-inhibit-pipelining nil
	   slime-autodoc-use-multiline-p t
	   slime-net-coding-system 'utf-8-unix)
     (add-hook 'slime-connected-hook 'slime-redirect-inferior-output)
     ;; select the contrib/extra packages we want
     (slime-setup '(slime-scratch slime-editing-commands slime-sbcl-exts
				  slime-fancy slime-repl slime-fontifying-fu))
     (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key slime-mode-map (kbd "C-j") 'newline)))

;; Scion: SLIME for Haskell
(sj/load-path-prepend "external/scion")
(autoload 'scion-mode "scion" nil t)
;;(setq scion-program "~/.cabal/bin/scion_server")

;; Haskell mode
(load (concat sj/emacs-base-dir "/external/haskell-mode/" "haskell-site-file"))
(autoload 'haskell-indentation-mode "haskell-indentation")
(setq haskell-program-name "ghci"
      inferior-haskell-wait-and-jump t
      haskell-indent-offset 4
      haskell-indent-look-past-empty-line nil
      haskell-font-lock-symbols nil)
(setq-default haskell-doc-show-global-types t)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(defun sj/haskell-mode-hook ()
  (setq comment-start   "--"
	comment-padding " ")
  (set (make-local-variable 'require-final-newline) t)
  (turn-on-haskell-decl-scan)
  (turn-on-haskell-doc-mode)
  ;; haskell-indent.el is too complex by far
  ;(turn-on-haskell-indent)
  ;; haskell-indentation.el seems nice & simple
  (haskell-indentation-mode)
  ;; from http://github.com/tibbe/haskell-style-guide
  (setq tab-width 4
	haskell-indentation-layout-offset 4
	haskell-indentation-left-offset 4
	haskell-indentation-ifte-offset 4)
  (auto-fill-mode)
  ;; Take haskell-indentation keys back from Viper's intrusive grip
  (setq viper-insert-local-user-map
	(sj/copy-keys-from-keymap haskell-indentation-mode-map
				  '([?\C-d] [backspace] [?\r]
				    ([backspace] . [delete])
				    ([backspace] . [?\d]))))
  (scion-mode 1)
  (scion-flycheck-on-save 1))
(add-hook 'haskell-mode-hook #'sj/haskell-mode-hook)
(add-to-list 'viper-emacs-state-mode-list 'inferior-haskell-mode)
(add-to-list 'filladapt-token-table '("-- " haskell-comment))
(add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
(add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
;; Select the inferior-haskell window when it's displayed.
(defun sj/select-inf-haskell-buffer ()
  "Display and select the buffer used by the inferior Haskell process, if any."
  (interactive)
  (when (processp (inferior-haskell-process))
    (let ((buf (process-buffer (inferior-haskell-process))))
      (select-window (display-buffer buf)))))
(eval-when-compile (require 'inf-haskell)) ;; for defadvice preactivation
(defadvice inferior-haskell-load-file (after sj/select-inferior-haskell
					     preactivate compile)
  "Display and select the inferior haskell buffer."
  (sj/select-inf-haskell-buffer))

;; Erlang mode
(setq erlang-root-dir "/opt/local/lib/erlang")
(require 'erlang-start)			; sets up autoloads

;; Distel -- Ditributed Emacs Lisp (Slime for Erlang!)
(sj/load-path-prepend "external/distel/elisp" "../doc")
(autoload 'distel-setup "distel")
(eval-after-load 'erlang
  '(distel-setup))

;; Rinari (load before ruby-mode, since Rinari has its own copy)
(sj/load-path-prepend "external/rinari" "doc")
(require 'rinari)
(defun sj/clone-rinari-keymap ()
  "Copy Rinari's difficult to reach keymaps to someplace better."
  (define-prefix-command 'sj/rinari-main-keymap)
  (define-prefix-command 'sj/rinari-jump-keymap)
  (set-keymap-parent 'sj/rinari-main-keymap
		     (lookup-key rinari-minor-mode-map (kbd "C-c ;")))
  (set-keymap-parent 'sj/rinari-jump-keymap
		     (lookup-key rinari-minor-mode-map (kbd "C-c ; f")))
  (define-key rinari-minor-mode-map [(super g)] sj/rinari-main-keymap)
  (define-key rinari-minor-mode-map [(super j)] sj/rinari-jump-keymap))
(eval-after-load 'rinari
  '(sj/clone-rinari-keymap))

;; Rhtml mode
(sj/load-path-prepend "external/rhtml")
(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist `(,(rx (and (or ".rhtml" ".html.erb") eol))
				. rhtml-mode))
(add-hook 'rhtml-mode-hook
     	  (defun sj/rhtml-mode-hook ()
	    (rinari-launch)))

;; Ruby
(setq ruby-deep-indent-paren nil
      ruby-deep-arglist nil)
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook (lambda () (highlight-parentheses-mode t))))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'viper-vi-state-mode-list 'ruby-mode)

;; An irb shell
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;; Electric minor mode for Ruby
(autoload 'ruby-electric-mode "ruby-electric" "Toggle Ruby Electric mode" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook
	     (defun sj/turn-on-ruby-electric-mode ()
	       (ruby-electric-mode t))))

;; An interface to Ruby's RDoc documentation
(sj/load-path-prepend "external/ri")
(autoload 'ri "ri" "Look up Ruby documentation." t)
(eval-after-load 'ri
  '(setq ri-repl-executable (concat sj/emacs-base-dir "/external/ri/ri_repl")))

;; CSS mode
(setq css-indent-offset 2)

;; YAML
(sj/load-path-prepend "external/yaml-mode")
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(eval-after-load 'yaml-mode
  '(add-hook 'yaml-mode-hook
	     (defun sj/yaml-mode-hook ()
	       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; cc-mode stuff
(eval-when-compile (require 'cc-mode))
(eval-when-compile (require 'dabbrev))
(defconst sj/c-style
  '((c-echo-syntactic-information-p 	. t)
    (c-tab-always-indent		. t)
    (c-hanging-comment-ender-p		. nil)
    (c-comment-continuation-stars	. "* ")
    (c-indent-comments-syntactically-p	. t)
    (c-cleanup-list			. (;brace-else-brace
					   ;empty-defun-braces
					   defun-close-semi
					   list-close-comma
					   scope-operator))
    (c-hanging-colons-alist		. ((case-label after)
					   (label after)))
    (c-hanging-braces-alist		. ((class-open after)
					   (inline-open after)
					   (brace-list-open)
					   (brace-list-intro before)
					   (brace-list-close)
					   (substatement-open after)
					   ;(topmost-intro after)
					   (block-close . c-snug-do-while)))
    (c-electric-pound-behavior 	. (alignleft))
    ;; copied from the K&R settings
    (c-basic-offset 		. 2)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist 		. ((statement-block-intro . 2)
				   (knr-argdecl-intro	  . 0)
				   (substatement-open	  . 0)
				   (inline-open		  . 0)
				   (inextern-lang	  . 0)
				   (statement-case-open	  . 0)
				   (label		  . -)
				   (statement-cont	  . +))))
  "Settings for my c-mode buffers.  Copy at your own risk.")
(defun sj/c-electric-comma ()
  "Add a newline after commas when preceded by braces.
See the docs for c-hanging-semi&comma-criteria."
  (if (and (eq last-command-event ?,)
           (save-excursion
             (backward-char)
             (skip-chars-backward " \t\n")
             (eq (char-before) ?})))
      t
    nil))
(defun sj/c-mode-common-hook ()
  (define-key c-mode-map "\C-m" 'newline-and-indent)
;  (define-key viper-insert-local-user-map [backspace] 'c-electric-backspace)
;  (define-key (current-local-map) [backspace] 'c-electric-backspace)
  (c-add-style "PERSONAL" sj/c-style t)
  (c-toggle-auto-hungry-state 1)
  (setq indent-tabs-mode nil)
  (pushnew 'sj/c-electric-comma c-hanging-semi&comma-criteria :test 'eq)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (setq fill-column 76)
  (auto-fill-mode 1)
  (turn-on-font-lock)
  (setq viper-insert-local-user-map c-mode-map)
  (setq imenu-create-index-function 'imenu-example--create-c-index))
(add-hook 'c-mode-common-hook 'sj/c-mode-common-hook)

;; cperl mode
(eval-when-compile (require 'cperl-mode))
(setq cperl-hairy                 nil
      cperl-electric-parens       "{"
      cperl-font-lock             t
      cperl-electric-lbrace-space nil
      cperl-electric-keywords     t
      cperl-electric-linefeed     t
      cperl-auto-newline          t
      ;; indentation
      cperl-indent-level           2
      cperl-brace-offset          -2
      cperl-brace-imaginary-offset 0
      cperl-label-offset          -2
      cperl-min-label-indent       1
      cperl-continued-statement-offset 2
      cperl-continued-brace-offset 0)
(add-hook 'cperl-mode-hook 'sj/cperl-mode-hook)
(defun sj/cperl-mode-hook ()
  (viper-mode)
  (setq viper-insert-local-user-map cperl-mode-map)
  (turn-on-font-lock)
  (highlight-parentheses-mode t)
  (setq fill-column 76)
  (local-set-key "\C-m" 'cperl-linefeed)
  (local-set-key "\C-j" 'newline-and-indent))
(mapc (lambda (mode-alist)
	(mapc (lambda (elt)	    ; prefer cperl-mode over perl-mode
		(when (eq (cdr elt) 'perl-mode)
		  (setcdr elt 'cperl-mode)))
	      (symbol-value mode-alist)))
      '(auto-mode-alist interpreter-mode-alist))

;; dmacro: dynamic macros
(sj/load-path-prepend "site-lisp/dmacro" t)
(require 'dmacro)
(dmacro-load (sj/emacs-path "dmacro/defaults.dm"))
(dmacro-load (sj/emacs-path "dmacro/elisp.dm"))
(dmacro-load (sj/emacs-path "dmacro/haskell.dm"))
(dmacro-load (sj/emacs-path "dmacro/makefile.dm"))
(dmacro-load (sj/emacs-path "dmacro/perl.dm"))
(dmacro-load (sj/emacs-path "dmacro/c.dm"))
(dmacro-load (sj/emacs-path "dmacro/c++.dm"))
(setq auto-dmacro-alist '(("\\.c\\(pp\\|xx\\|c\\)?$" . c_masthead)
			  ("\\.h\\(pp\\|xx\\|h\\)?$" . h_masthead)
			  ("\\.x$" . rpc_masthead)
			  ("." . masthead)))

;; RPC .x files
(push '("\\.x$" . c-mode) auto-mode-alist)

;; imenu
(setq imenu-sort-function 'imenu--sort-by-name)

;; Load CEDET
;(setq semantic-load-turn-everything-on t)
;; (setq semantic-imenu-bucketize-file nil
;;       semantic-imenu-expand-type-members nil
;;       semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
;; (load-file (sj/emacs-path "external/cedet/common/cedet.el"))
;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)
;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;(semantic-load-enable-gaudy-code-helpers)
;; * This turns on which-func support (Plus all other code helpers)
;(semantic-load-enable-excessive-code-helpers)
;; add header file mappings for c/c++
;; (mapc #'(lambda (dir)
;; 	  (when (file-directory-p dir)
;; 	    (semantic-add-system-include dir 'c-mode)
;; 	    (semantic-add-system-include dir 'c++-mode)))
;;       '("/opt/local/include" "/usr/local/include" "/usr/include"))
;;
;; (mapc #'(lambda (dir)
;; 	  (when (file-directory-p dir)
;; 	    (message dir)
;; 	    (add-to-list 'Info-additional-directory-list dir)))
;;       (list
;;        (sj/emacs-path "external/cedet/ede")
;;        (sj/emacs-path "external/cedet/eieio")
;;        (sj/emacs-path "external/cedet/semantic/doc")
;;        (sj/emacs-path "external/cedet/speedbar")))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
