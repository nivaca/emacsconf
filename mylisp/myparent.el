;;; mylisp/myparent.el -*- lexical-binding: t; -*-

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :hook(
        (prog-mode . rainbow-delimiters-mode)
        (latex-mode . rainbow-delimiters-mode)
        )
  )



(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)

  ;; safer minibuffer behavior
  (add-hook 'minibuffer-setup-hook #'smartparens-mode)

  (show-smartparens-global-mode t)
  (smartparens-global-mode t)

  ;; ensure skipping instead of overwriting
  (setq sp-autoskip-closing-pair 'always)
  :blackout)

(show-paren-mode 1)



(provide 'myparent)
