;;; mylisp/myparent.el -*- lexical-binding: t; -*-



;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :config
  (rainbow-delimiters-mode)
)


(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (show-smartparens-global-mode t) ;; show pairs by blinking one
  (smartparens-global-mode t)
  )


(provide 'myparent)
