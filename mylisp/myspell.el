;;; mylisp/mydisplay.el -*- lexical-binding: t; -*-

;; ==================== Jinx ====================
;; Required in Fedora: enchant2-devel, pkgconf
(use-package jinx
  :straight t
  ;; :hook ((LaTeX-mode . jinx-mode)
  ;;        (latex-mode . jinx-mode)
  ;;        (markdown-mode . jinx-mode)
  ;;        (org-mode . jinx-mode)
  ;;        (text-mode . jinx-mode)
  ;;        ;; (emacs-startup . global-jinx-mode)
  ;;        (emacs-startup . (lambda ()
  ;;                           (global-jinx-mode)
  ;;                           (setq jinx-languages "es_CO"))))
  :bind
  ( :map global-map
    ("M-$" . jinx-correct) ; or bind `jinx-correct-all'
    ("C-M-$" . jinx-languages))
  ;; ([remap ispell-word] . jinx-correct)
  :config
  (setq jinx-languages "EN_US ES_CO la")
  (setq jinx-exclude-faces
        '((prog-mode font-lock-comment-face font-lock-string-face)))
  (cl-pushnew 'font-lock-comment-face (alist-get 'tex-mode jinx-exclude-faces))
  (blackout 'jinx-mode)
  (global-jinx-mode 1)
  )

(provide 'myspell)
