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
    :commands lsp)

(provide 'mylsp)
