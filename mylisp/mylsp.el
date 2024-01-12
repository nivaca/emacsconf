;;; mylisp/mylsp.el -*- lexical-binding: t; -*-

(use-package eglot
  :straight t
  :defer t
  :hook ((latex-mode . eglot-ensure)
         (LaTeX-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-f" . eglot-format-buffer))
  )

(use-package consult-eglot
  :straight t)


;; (use-package eglot-tempel
;;   :disabled
;;   :straight t
;;   :after eglot
;;   )

(provide 'mylsp)
