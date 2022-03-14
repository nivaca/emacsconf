;;; mylisp/myprojectile.el -*- lexical-binding: t; -*-

;; ============== projectile ==============
(use-package projectile
  :straight t
  :config
  (setq projectile-indexing-method 'alien)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :blackout (projectile-mode . " ¶")

  )




;; ripgrep support
(use-package rg
  :straight t)

(provide 'myprojectile)
