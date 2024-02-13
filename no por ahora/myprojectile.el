;;; mylisp/myprojectile.el -*- lexical-binding: t; -*-

;; ============== projectile ==============
(use-package projectile
  :disabled
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; ripgrep support
(use-package rg
  :straight t)

(provide 'myprojectile)
