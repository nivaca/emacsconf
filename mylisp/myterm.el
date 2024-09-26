;;; mylisp/myterm.el -*- lexical-binding: t; -*-

(use-package eat
  :straight
  (eat
   :type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))
   )
  :config
  (setq process-adaptive-read-buffering nil)
  )

(provide 'myterm)
