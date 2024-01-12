;;; mylisp/myhyp.el -*- lexical-binding: t; -*-

(use-package hyperbole
  :straight
  (hyperbole
   :host nil
   :repo "https://git.savannah.gnu.org/git/hyperbole.git")
  ;; :config
  ;; (hyperbole-mode 1)
  `:bind
  ("C-=" . hui-select-thing)
  )


(provide 'myhyp)
