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
    (setcdr (assq 'output-pdf TeX-view-program-selection) '("okular"))))




(with-eval-after-load 'font-latex
  (dolist (face '(font-latex-sectioning-0-face
                  font-latex-sectioning-1-face
                  font-latex-sectioning-2-face
                  font-latex-sectioning-3-face
                  font-latex-sectioning-4-face
                  font-latex-sectioning-5-face
                  font-latex-slide-title-face
                  font-latex-bold-face
                  font-latex-italic-face
                  font-latex-math-face
                  font-latex-string-face
                  font-latex-warning-face
                  font-latex-sedate-face
                  font-latex-verbatim-face))
    (set-face-attribute face nil 
                        :family 'unspecified 
                        :slant 'normal      ; Add this
                        :inherit 'default))
  ;; Disable font scaling for sectioning commands
  (setq font-latex-fontify-sectioning 1.0)
  ;; Optional: also disable script font changes (sub/superscripts)
  (setq font-latex-fontify-script nil)
  )


(provide 'myauctex)
