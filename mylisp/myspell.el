;;; mylisp/mydisplay.el -*- lexical-binding: t; -*-

;; Required in Fedora: enchant2-devel, pkgconf
(use-package jinx
  :straight t
  :hook ((LaTeX-mode . jinx-mode)
         (latex-mode . jinx-mode)
         ;; (markdown-mode . jinx-mode)
         ;; (org-mode . jinx-mode)
         )
  ;; :hook (emacs-startup . global-jinx-mode)
  :config
  (blackout 'jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct))


;; --------------------------------------------------
;; Reverso: spelling & grammar check, synonyms, etc. 
(use-package reverso
  :straight
  (:host github :repo "SqrtMinusOne/reverso.el"))

(provide 'myspell)
