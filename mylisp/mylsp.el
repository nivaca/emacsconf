;;; mylisp/mylsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((LaTeX-mode . lsp)
         (latex-mode . lsp))
  :custom
  (lsp-enable-snippet nil)
  :config
  (yas-global-mode nil)
  )


(use-package lsp-snippet-tempel
  :straight (lsp-snippet-tempel :type git
                                :host github
                                :repo "svaante/lsp-snippet")
  :config
  (when (featurep 'lsp-mode)
    ;; Initialize lsp-snippet -> tempel in lsp-mode
    (lsp-snippet-tempel-lsp-mode-init))
  (when (featurep 'eglot)
    ;; Initialize lsp-snippet -> tempel in eglot
    (lsp-snippet-tempel-eglot-init)))


;; yasnippet
(use-package yasnippet
  :disabled
  :straight t
  :config
  (yas-global-mode -1)
  )


;; treesit
(use-package treesit-auto
  :disabled
  :straight t 
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(provide 'mylsp)
