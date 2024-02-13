;;; mylisp/mylsp.el -*- lexical-binding: t; -*-

(use-package eglot
  :straight
  :defer t
  :hook (
         ;; (latex-mode . eglot-ensure)
         ;; (LaTeX-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         )
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-f" . eglot-format-buffer))
  )

(use-package consult-eglot
  :straight t)


(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'mylsp)
