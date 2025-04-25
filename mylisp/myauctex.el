;;; mylisp/myauctex.el -*- lexical-binding: t; -*-

(use-package tex
  :straight auctex
  :ensure auctex
  :mode
  ("\\.tex\\'" . latex-mode)
  ;; ("\\.ltx\\'" . latex-mode)
  :commands
  (latex-mode LaTeX-mode TeX-mode)
  :init
  (setq-default TeX-master nil)
  (defun nv-setup-auctex ()
    "Sets AUCTeX up."
    (interactive)
    (message "Setting up AUCTeX...")
    ;; (LaTeX-preview-setup)
    (jinx-mode)
    (outline-minor-mode)
    ;; (hs-minor-mode)
    )
  :hook
  (LaTeX-mode . nv-setup-auctex)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-PDF-mode t)
  (LaTeX-beamer-item-overlay-flag nil)
  (TeX-PDF-mode t)
  (TeX-insert-macro-default-style 'mandatory-args-only)
  ;;
  :config
  (use-package reftex
    :straight t
    :commands turn-on-reftex
    :config
    (reftex-plug-into-AUCTeX)
    :blackout reftex-mode)
  ;;
  (use-package auctex-latexmk
    :straight t
    :after tex
    :custom
    (auctex-latexmk-inherit-TeX-PDF-mode t)
    :config
    (auctex-latexmk-setup))
  )



;; Other auctex settings ---------------------------------------------
(use-package emacs
  :straight
  :after tex
  :config
  (with-eval-after-load "tex"
    (add-to-list 'TeX-view-program-list '("okular" "/usr/bin/okular %o"))
    (setcdr (assq 'output-pdf TeX-view-program-selection) '("okular")))
  )


(provide 'myauctex)
