;;; mylisp/mymarkdown.el -*- lexical-binding: t; -*-

(use-package markdown-mode
  :straight t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :blackout  (markdown-mode . " MD")
:defer t
:config
)

(provide 'mymarkdown)
