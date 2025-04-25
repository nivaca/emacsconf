;;; mylisp/mydisplay.el -*- lexical-binding: t; -*-

;; ==================== Jinx ====================
;; Required in Fedora: enchant2-devel, pkgconf
(use-package jinx
  :straight t
  :hook ((LaTeX-mode . jinx-mode)
         (latex-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (org-mode . jinx-mode)
         (text-mode . jinx-mode)
         ;; (emacs-startup . global-jinx-mode)
         )
  :config
  (cl-pushnew 'font-lock-comment-face (alist-get 'tex-mode jinx-exclude-faces))
  (blackout 'jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct))

(provide 'myspell)
