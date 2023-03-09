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
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (show-smartparens-global-mode t) ;; show pairs by blinking one
  (smartparens-global-mode t)
  (sp-pair "\"" "\""
           :trigger "\""
           :unless '(sp-point-before-word-p
                     sp-point-after-word-p))
  (sp-pair "(" ")"
           :trigger "("
           :unless '(sp-point-before-word-p
                     sp-point-after-word-p))
  (sp-pair "[" "]"
           :trigger "["
           :unless '(sp-point-before-word-p
                     sp-point-after-word-p))
  (sp-pair "{" "}"
           :trigger "{"
           :unless '(sp-point-before-word-p
                     sp-point-after-word-p))
  )

;; important: 
(show-paren-mode 1)


(provide 'myparent)
