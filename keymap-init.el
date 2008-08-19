;;; keyboard-init --- keyboard settings    [sj 95/06/11]
;;;
;;; $Id: keymap-init.el,v 1.11 1999/06/06 06:08:07 sj Exp $

(when user-sj-p
  (global-set-key [?\C-x ?f] 'find-file) ; set-fill-column is a pita
  (global-set-key [insert]
		  #'(lambda () 
		      (interactive) 
		      (ding) 
		      (message "You didn't mean to hit this key!"))))

;; private keymap for my own stuff
(define-prefix-command 'sj/private-keymap)
(global-set-key [?\C-c ?\,] sj/private-keymap)

;; Hyper keymap 
;(global-set-key [(hyper l)]   'dmacro-wrap-line)
;(global-set-key [(hyper r)]   'dmacro-wrap-region)
;(global-set-key [(hyper u)]   'advertised-undo)

(define-key sj/private-keymap [?a] 'add-time-log-entry-other-window)
(define-key sj/private-keymap [?c] 'cvs-update)
(define-key sj/private-keymap [?d] 'insert-dmacro)
(define-key sj/private-keymap [?g] 'agrep)
(define-key sj/private-keymap [?i] 'insert-buffer)
(define-key sj/private-keymap [?l] 'load-library)
(define-key sj/private-keymap [?k] 'bury-buffer)
(define-key sj/private-keymap [?L] 'locate-library)
(define-key sj/private-keymap [?m] 'compile)
(define-key sj/private-keymap [?w] 'write-region)
(define-key sj/private-keymap [?\,] 'bbdb-complete-name)
(define-key sj/private-keymap [?\.] 'PC-lisp-complete-symbol)
(define-key sj/private-keymap [?\;] 'comment-region)
;(define-key sj/private-keymap [(hyper s)] 'sj/swap-window-positions)

;; other redefinitions
;; 
;(global-set-key [return] 'newline-and-indent)
(global-set-key [?\C-x ?4 ?n] 'sj/swap-window-positions)
(global-set-key [?\C-c ?s] 'dictionary-search)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
