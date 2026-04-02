;;; mylisp/myparent.el -*- lexical-binding: t; -*-

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :hook(
        (prog-mode . rainbow-delimiters-mode)
        (LaTeX-mode . rainbow-delimiters-mode)
        )
  )


(use-package smartparens
  :straight t
  :custom
  (sp-autoskip-closing-pair 'conservative)
  :config
  (require 'smartparens-config)
  ;; safer minibuffer behavior
  (add-hook 'minibuffer-setup-hook #'smartparens-mode)
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  :blackout)

(with-eval-after-load 'smartparens-latex
  (sp-local-pair 'LaTeX-mode "$" nil :actions nil))



(show-paren-mode 1)


(provide 'myparent)
