;;; mylisp/myparent.el -*- lexical-binding: t; -*-



;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :config
  (rainbow-delimiters-mode)
)

(use-package smartparens
  :straight t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (setq sp-ignore-modes-list '(nxml-mode)
        sp-ignore-modes-list
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (show-smartparens-global-mode 1) ; show pairs by blinking one
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  )

(provide 'myparent)
