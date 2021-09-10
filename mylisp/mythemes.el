;;; mylisp/mythemes.el -*- lexical-binding: t; -*-

;; (setq custom-theme-directory (concat user-emacs-directory "otherlisp/themes"))
;; (load-theme 'gnome2 t)

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
  ;; :disabled
  :straight t
  :config
  (load-theme 'base16-flat t))



(use-package doom-themes
  :disabled
  :straight t
  :custom
  (doom-nord-brighter-comments t)
  (doom-one-brighter-comments t)
  (doom-one-brighter-modeline t)
  :init
  (doom-themes-visual-bell-config)
  (load-theme 'doom-palenight t)
  )


(use-package color-theme-sanityinc-tomorrow
  :disabled
  :straight t
  :init
  (load-theme 'sanityinc-tomorrow-night t)
  )


(provide 'mythemes)
