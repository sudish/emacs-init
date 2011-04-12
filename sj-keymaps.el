;;; sj-keymaps --- Personal keymaps
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; global bindings for everyday stuff
(define-key global-map [(super f1)]    'sj/tile-or-rotate-frames)
(define-key global-map [(super A)]     'align-regexp)
(define-key global-map [(super m)]     'magit-status)
(define-key global-map [(super r)]     'rgrep)
(define-key global-map [(super R)]     'sj/project-root-rgrep)
(define-key global-map [(super t)]     'sj/anything-textmate)
(define-key global-map [(super T)]     'sj/anything-code-nav)
(define-key global-map [(super u)]     'undo)
(define-key global-map [(super :)]     'eval-expression)
(define-key global-map [(super \0)]    'delete-window)
(define-key global-map [(super \1)]    'delete-other-windows)
;; (define-key global-map [(super up)]    'windmove-up)
;; (define-key global-map [(super down)]  'windmove-down)
;; (define-key global-map [(super left)]  'windmove-left)
;; (define-key global-map [(super right)] 'windmove-right)
(define-key global-map [(super up)]
  (defun sj/previous-window ()
    (interactive)
    (other-window -1)))
(define-key global-map [(super down)]  'other-window)
(define-key global-map [(super left)]  'winner-undo)
(define-key global-map [(super right)] 'winner-redo)
(define-key global-map (kbd "C-;") 'iedit-mode)

;; private keymap for useful, but less commonly used stuff
(define-prefix-command 'sj/private-keymap)
(global-set-key [(super .)] 'sj/private-keymap)

(define-key sj/private-keymap [(super .)] 'anything)

(define-key sj/private-keymap [?i] 'insert-buffer)
(define-key sj/private-keymap [?l] 'load-library)
(define-key sj/private-keymap [?L] 'locate-library)
(define-key sj/private-keymap [?k] 'bury-buffer)
(define-key sj/private-keymap [?m] 'compile)
(define-key sj/private-keymap [?s] 'sj/swap-window-positions)

;; the default set-fill-column binding is annoying
(global-set-key [?\C-x ?f] 'find-file)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
