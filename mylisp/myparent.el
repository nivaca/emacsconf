;;; mylisp/myparent.el -*- lexical-binding: t; -*-

;; Highlight matching delimiters, even when the match is off-screen.

(use-package emacs
  :custom
  ;; Highlight matching delimiters, even when the match is off-screen.
  (show-paren-context-when-offscreen t)
  ;; Highlight the entire expression when point is on a delimiter.
  (show-paren-style 'mixed)
  :config
  (show-paren-mode 1))


;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :hook ((prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode))
  )


(use-package smartparens
  :straight t
  :custom
  ;; Skip over closing delimiters conservatively.
  (sp-autoskip-closing-pair 'conservative)
  :hook ((prog-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode)
         ;; Structural editing shines in Lisp.
         (emacs-lisp-mode . smartparens-strict-mode)
         (lisp-interaction-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  :blackout)


(with-eval-after-load 'smartparens-latex
  ;; Don't auto-pair '$' in LaTeX.
  (sp-local-pair 'LaTeX-mode "$" nil :actions nil))


(provide 'myparent)
