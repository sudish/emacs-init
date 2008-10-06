;;; lang-init --- various language-specific modes   [sj--95/11/06]
;;;

;; makefile-mode
(push '("[mM]akefile$" . makefile-mode) auto-mode-alist)

;; eldoc: automatic docs in minibuffer
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Clojure and SLIME
(load-path-prepend '("~/gnuemacs/external/swank-clojure"
		     "~/gnuemacs/external/slime"
		     "~/gnuemacs/external/clojure-mode"))
(setq swank-clojure-jar-path "/opt/local/share/java/clojure/lib/clojure.jar")
(require 'clojure-auto)
(require 'clojure-paredit)
(require 'swank-clojure-autoload)
(require 'slime-autoloads)
(slime-setup '(slime-scratch slime-editing-commands))
(defun run-clojure ()
  "Starts clojure in slime"
  (interactive)
  (slime 'clojure))

;; cc-mode stuff
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
				   (statement-cont	  . +)))
    )
  "Settings for my c-mode buffers.  Copy at your own risk.")
(defun sj/c-electric-comma ()
  "Add a newline after commas when preceded by braces.
See the docs for c-hanging-semi&comma-criteria."
  (if (and (eq last-command-char ?,)
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
  (setq fill-column 76)
  (local-set-key "\C-m" 'cperl-linefeed)
  (local-set-key "\C-j" 'newline-and-indent))
(mapc (lambda (x) 
	(and (eq (cdr x) 'perl-mode)
	     (setcdr x 'cperl-mode)))
      auto-mode-alist)

;; dmacro: dynamic macros
(when user-sj-p
  (require 'dmacro)
  (dmacro-load "~/gnuemacs/dmacro/defaults.dm")
  (dmacro-load "~/gnuemacs/dmacro/elisp.dm")
  (dmacro-load "~/gnuemacs/dmacro/makefile.dm")
  (dmacro-load "~/gnuemacs/dmacro/perl.dm")
  (dmacro-load "~/gnuemacs/dmacro/c.dm")
  (dmacro-load "~/gnuemacs/dmacro/c++.dm")
  (setq auto-dmacro-alist '(("\\.c\\(pp\\|xx\\|c\\)?$" . c_masthead)
			    ("\\.h\\(pp\\|xx\\|h\\)?$" . h_masthead)
			    ("\\.x$" . rpc_masthead)
			    ("." . masthead))))

;; RPC .x files
(push '("\\.x$" . c-mode) auto-mode-alist)

;; imenu
(setq imenu-sort-function 'imenu--sort-by-name)

;; Load CEDET
;(setq semantic-load-turn-everything-on t)
(setq semantic-imenu-bucketize-file nil
      semantic-imenu-expand-type-members nil
      semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
(load-file "~/gnuemacs/site-lisp/cedet/common/cedet.el")
;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)
;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;(semantic-load-enable-gaudy-code-helpers)
;; * This turns on which-func support (Plus all other code helpers)
;(semantic-load-enable-excessive-code-helpers)
(mapc (lambda (dir)
	(semantic-add-system-include dir 'c-mode)
	(semantic-add-system-include dir 'c++-mode))
      '("/opt/local/include" "/usr/include"))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
