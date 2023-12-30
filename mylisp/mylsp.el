;;; mylisp/mylsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
    :straight t
    :ensure t
    :hook
    ((lisp-mode . lsp)
     (LaTeX-mode . lsp)
     (TeX-mode . lsp)
     (bibtex-mode . lsp)
     )
    :custom
    (lsp-enable-snippet nil)
    :commands lsp
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

(provide 'mylsp)
