;;; mythemes.el -*- lexical-binding: t; -*

;; (message "»»»»»»»»»»»» Loading mythemes.el ««««««««««««« ")


;; (load-theme 'adwaita t)
;; (load-theme 'modus-operandi-tinted t)



(use-package ef-themes
  ;; :disabled
  :straight
  (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
  (load-theme 'ef-maris-light t)
  ;; (load-theme 'ef-elea-dark t)
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
  (doom-dark+-blue-modeline t)
  (doom-dark+-padded-modeline nil)
  :config
  (doom-themes-visual-bell-config)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-spacegray t)
  ;; (load-theme 'doom-tokyo-night t)
  (load-theme 'doom-dark+ t)
  )


(use-package color-theme-sanityinc-tomorrow
  :disabled
  :straight t
  :config
  (load-theme 'color-theme-sanityinc-tomorrow-day t)
  )

(use-package base16-theme
  :disabled
  :straight t
  :config
  (load-theme 'base16-equilibrium-gray-light t)
  )




;; --------------------------------------------------------------
(provide 'mythemes)
