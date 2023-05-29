;;; mylisp/mythemes.el -*- lexical-binding: t; -*-

;; (load-theme 'tango t)

(use-package nordic-night-theme
  :disabled
  :straight (:type git :repo "https://git.sr.ht/~ashton314/nordic-night" :branch "main")
  :init
  (load-theme 'nordic-night t)
)



(use-package vscode-dark-plus-theme
  ;; :disabled
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
  (doom-oceanic-next-comment-bg nil)
  (doom-oceanic-next-brighter-comments nil)
  (doom-feather-light-brighter-modeline t)
  (doom-feather-light-brighter-comments t)
  (doom-feather-light-padded-modeline t)
  :init
  (doom-themes-visual-bell-config)
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-feather-light t)
  )

(use-package ef-themes
  :disabled
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
  (load-theme 'ef-winter t)
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
