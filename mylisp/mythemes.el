;;; mylisp/mythemes.el -*- lexical-binding: t; -*-


(use-package vscode-dark-plus-theme
  :disabled
  :straight t
  :config
  (load-theme 'vscode-dark-plus t))


(use-package doom-themes
  :disabled
  :straight t
  :custom
  (doom-nord-brighter-comments t)
  (doom-one-brighter-comments t)
  (doom-dracula-brighter-comments t)
  (doom-oceanic-next-comment-bg nil)
  (doom-oceanic-next-brighter-comments nil)
  :init
  (doom-themes-visual-bell-config)
  (load-theme 'doom-one t)
  )


(use-package ef-themes
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
  (load-theme 'ef-winter t)
  )




(provide 'mythemes)
