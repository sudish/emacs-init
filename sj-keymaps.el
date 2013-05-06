;;; sj-keymaps --- Personal keymaps
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; global bindings for everyday stuff
(define-key global-map [(super f1)]	'sj/tile-or-rotate-frames)
(define-key global-map [(super A)]	'align-regexp)
(define-key global-map [(super m)]	'magit-status)
(define-key global-map [(super \])]     'highlight-symbol-next)
(define-key global-map [(super \[)]     'highlight-symbol-prev)
(define-key global-map [(super r)]	'ack)
(define-key global-map [(super R)]	'rgrep)
;;(define-key global-map [(super t)]	'sj/anything-textmate)
(define-key global-map [(super t)]	'sj/peepopen-goto-file-gui)
(define-key global-map [(super T)]	'sj/anything-code-nav)
(define-key global-map [(super u)]	'undo)
(define-key global-map [(super :)]	'eval-expression)
(define-key global-map [(super =)]	'text-scale-adjust)
(define-key global-map [(super -)]	'text-scale-adjust)
;(define-key global-map [(super \0)]	'text-scale-adjust)
(define-key global-map [(super \0)]	'delete-window)
(define-key global-map [(super \1)]	'delete-other-windows)
(define-key global-map [(super d)]	'split-window-horizontally)
(define-key global-map [(super D)]	'split-window-vertically)
(define-key global-map [(super left)]
  (defun sj/previous-window ()
    (interactive)
    (other-window -1)))
(define-key global-map [(super right)] 'other-window)
(define-key global-map [(super down)]  'winner-undo)
(define-key global-map [(super up)]    'winner-redo)
(define-key global-map [(control \;)]  'iedit-mode)

;; private keymap for useful, but less commonly used stuff
(define-prefix-command 'sj/private-keymap)
(global-set-key [(super .)] 'sj/private-keymap)

(define-key sj/private-keymap [(super .)] 'anything)

(define-key sj/private-keymap [(super ?f)] 'follow-delete-other-windows-and-split)
(define-key sj/private-keymap [?d]         'dash-at-point)
(define-key sj/private-keymap [?i]         'insert-buffer)
(define-key sj/private-keymap [?l]         'load-library)
(define-key sj/private-keymap [?L]         'locate-library)
(define-key sj/private-keymap [?k]         'bury-buffer)
(define-key sj/private-keymap [(super ?m)] 'minimap-create)
(define-key sj/private-keymap [?s]         'sj/swap-window-positions)

;; the default set-fill-column binding is annoying
(global-set-key [?\C-x ?f] 'find-file)

;; anything-specific prefix
(define-prefix-command 'sj/anything-keymap)
(global-set-key [(super ?,)] 'sj/anything-keymap)

(define-key sj/anything-keymap [(super ?,)] 'anything)
(define-key sj/anything-keymap [(super ?m)] 'anything-mini)
(define-key sj/anything-keymap [(super ?i)] 'anything-imenu)
(define-key sj/anything-keymap [(super ?o)] 'anything-occur)
(define-key sj/anything-keymap [(super ?n)] 'anything-resume)
(define-key sj/anything-keymap [(super ?x)] 'anything-M-x)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
