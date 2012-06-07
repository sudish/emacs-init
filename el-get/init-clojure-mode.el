;;; init-clojure-mode ---    [sj--12/06/01]

(setq clojure-mode-use-backtracking-indent t
      clojure-mode-font-lock-comment-sexp t)

(add-to-list 'viper-vi-state-mode-list 'clojure-mode)
