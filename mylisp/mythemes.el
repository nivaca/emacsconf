;;; mylisp/mythemes.el -*- lexical-binding: t; -*-

;; (load-theme 'leuven t)

(use-package kaolin-themes
  :disabled
  :straight t
  :config
  (load-theme 'kaolin-mono-light t)
  )

(use-package tron-legacy-theme
  :disabled
  :straight t
  :config
  (setq tron-legacy-theme-softer-bg t)
  (setq tron-legacy-theme-dark-fg-bright-comments nil)
  (load-theme 'tron-legacy t))


(use-package vscode-dark-plus-theme
  :disabled
  :straight t
  :config
  (load-theme 'vscode-dark-plus t))


(use-package base16-theme
  :disabled
  :straight t
  :config
  (load-theme 'base16-flat t))


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
