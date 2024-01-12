;;; mylisp/mythemes.el -*- lexical-binding: t; -*-

(use-package nimbus-theme
  :disabled
  :straight t
  :init (load-theme 'nimbus t)
  )

(use-package vscode-dark-plus-theme
  :disabled
  :config
  (load-theme 'vscode-dark-plus t)
  )

(use-package doom-themes
  :disabled
  :straight t
  :custom
  (doom-nord-brighter-comments t)
  (doom-one-brighter-comments t)
  (doom-dracula-brighter-comments t)
  (doom-city-lights-brighter-comments nil)
  (doom-tokyo-night-brighter-comments nil)
  :init
  (doom-themes-visual-bell-config)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-city-lights t)
  (load-theme 'doom-tokyo-night t)
  )


(use-package ef-themes
  ;; :disabled
  :straight
  (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
  (load-theme 'ef-maris-dark t)
  )


(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config
  (load-theme 'color-theme-sanityinc-tomorrow-day t)
  )

(use-package base16-theme
  :disabled
  :config
  (load-theme 'base16-equilibrium-gray-light t)
  )

;; --------------------------------------------------------------
(provide 'mythemes)
