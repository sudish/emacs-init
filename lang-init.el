;;; lang-init --- various language-specific modes   [sj--95/11/06]
;;;

;; sql
(setq sql-product 'postgres)

;; makefile-mode
(push '("[mM]akefile$" . makefile-mode) auto-mode-alist)

;; eldoc: automatic docs in minibuffer
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Clojure, swank
(sj/load-path-prepend (list (sj/emacs-path 'ext "swank-clojure")
			    (sj/emacs-path 'ext "clojure-mode")))

(setq clojure-mode-use-backtracking-indent t
      clojure-mode-font-lock-comment-sexp t)
(require 'clojure-auto)
(autoload 'clojure-indent-function "clojure-mode") ; for use in slime buffers
(require 'clojure-paredit)
(require 'swank-clojure-autoload)
(setq swank-clojure-jar-path "~/src/git/clojure/clojure.jar")
(setq swank-clojure-extra-classpaths
      '("~/src/git/clojure-contrib/clojure-contrib.jar"
	"~/.clojure/*.jar"))

;; SBCL
(setq sj/slime-sbcl-path "/opt/local/bin/sbcl")
(eval-after-load 'slime
  '(add-to-list 'slime-lisp-implementations
		`(sbcl (,sj/slime-sbcl-path) :coding-system utf-8-unix)))

;; SLIME -- LISP blissage
(sj/load-path-prepend (sj/emacs-path 'ext "slime"))
(require 'slime-autoloads)
(eval-after-load 'slime
  '(progn
     (slime-setup '(slime-scratch slime-editing-commands slime-fancy))
     (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key slime-mode-map (kbd "C-j") 'newline)))

;; Rinari
(sj/load-path-prepend (sj/emacs-path 'ext "rinari"))
(require 'rinari)

;; ruby
(require 'ruby-mode)
(require 'ruby-electric)
(require 'inf-ruby)
(push '("\\.rb$" . ruby-mode) auto-mode-alist)

;; cc-mode stuff
(eval-when-compile
  (require 'cc-mode))
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
(eval-when-compile
  (require 'cperl-mode))
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
(mapc #'(lambda (x)			; prefer cperl-mode over perl-mode
	  (and (eq (cdr x) 'perl-mode)
	       (setcdr x 'cperl-mode)))
      auto-mode-alist)

;; dmacro: dynamic macros
(require 'dmacro)
(dmacro-load (sj/emacs-path "dmacro" "defaults.dm"))
(dmacro-load (sj/emacs-path "dmacro" "elisp.dm"))
(dmacro-load (sj/emacs-path "dmacro" "makefile.dm"))
(dmacro-load (sj/emacs-path "dmacro" "perl.dm"))
(dmacro-load (sj/emacs-path "dmacro" "c.dm"))
(dmacro-load (sj/emacs-path "dmacro" "c++.dm"))
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
(setq semantic-imenu-bucketize-file nil
      semantic-imenu-expand-type-members nil
      semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
(load-file (sj/emacs-path 'ext "cedet/common/cedet.el"))
;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)
;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;(semantic-load-enable-gaudy-code-helpers)
;; * This turns on which-func support (Plus all other code helpers)
;(semantic-load-enable-excessive-code-helpers)
;; add header file mappings for c/c++
(mapc #'(lambda (dir)
	  (when (file-directory-p dir)
	    (semantic-add-system-include dir 'c-mode)
	    (semantic-add-system-include dir 'c++-mode)))
      '("/opt/local/include" "/usr/local/include" "/usr/include"))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
