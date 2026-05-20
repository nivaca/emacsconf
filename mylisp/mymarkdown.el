;;; mylisp/mymarkdown.el -*- lexical-binding: t; -*-

(use-package markdown-mode
  :straight t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :blackout  (markdown-mode . " MD")
  :defer t
  )

(provide 'mymarkdown)
