;;; mythemes.el -*- lexical-binding: t; -*

;; (message "»»»»»»»»»»»» Loading mythemes.el ««««««««««««« ")

(nv-disable-custom-themes)

;; (load-theme 'misterioso t)
;; (load-theme 'deeper-blue t)

(use-package modus-themes
  ;; :disabled
  :straight t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  (modus-themes-load-theme 'modus-vivendi-tinted)
  )

(use-package nano-theme
  :disabled
  :straight
  (nano-theme :type git :host github :repo "rougier/nano-theme")
  :config
  (load-theme 'nano-dark t))

(use-package turbo-c-nv-theme
  :disabled
  :straight nil
  :load-path "~/emacs/themes/"
  :config
  (load-theme 'turbo-c-nv t))


(use-package color-theme-modern
  :disabled
  :straight t
  :config
  ;; (load-theme 'classic t)
  (load-theme 'gnome2 t)
  ;; (load-theme 'cobalt t)
  ;; (load-theme 'subtle-hacker t)
  )



(use-package tomorrow-night-deepblue-theme
  :disabled
  :straight t
  :config
  ;; Disable all themes and load the Tomorrow Night Deep Blue theme
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the tomorrow-night-deepblue theme
  (load-theme 'tomorrow-night-deepblue t))


(use-package doric-themes
  :disabled
  :straight
  (ef-themes :type git :host github :repo "protesilaos/doric-themes")
  :config
  (load-theme 'doric-mermaid t)
  )


(use-package ef-themes
  :disabled
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
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-dark+ t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-spacegray t)
  ;; (load-theme 'doom-tokyo-night t)
  (load-theme 'doom-one t)
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
  (load-theme 'base16-eva t)
  )




;; --------------------------------------------------------------
(provide 'mythemes)
