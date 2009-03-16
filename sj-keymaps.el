;;; sj-keymaps --- Personal keymaps
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; some global bindings for everyday stuff
(define-key global-map [(super f1)]   'anything)
(define-key global-map [(super m)]    'magit-status)
(define-key global-map [(super r)]    'rgrep)
(define-key global-map [(super u)]    'undo)
(define-key global-map [(super :)]    'eval-expression)
(define-key global-map [(super \0)]   'delete-window)
(define-key global-map [(super \1)]   'delete-other-windows)
(define-key global-map [(super up)]   'windmove-up)
(define-key global-map [(super down)] 'windmove-down)

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
