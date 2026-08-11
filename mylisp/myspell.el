;;; mylisp/myspell.el -*- lexical-binding: t; -*-

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
  (setq jinx-languages "en_US es_CO la")

  ;; NOTE: `jinx-exclude-faces' is *extended*, never overwritten — the
  ;; defaults carry sensible entries for org, markdown, prog-mode, etc.
  ;;
  ;; AUCTeX's `LaTeX-mode' derives from `TeX-mode' → `text-mode', not from
  ;; the built-in `tex-mode', so Jinx's `tex-mode' defaults never apply in
  ;; AUCTeX buffers.  This entry is what stops \enquote and friends from
  ;; being checked; `font-latex-sedate-face' covers generic control
  ;; sequences, including macros you define yourself.
  (setf (alist-get 'LaTeX-mode jinx-exclude-faces)
        '(font-latex-math-face
          font-latex-sedate-face
          font-latex-verbatim-face
          font-latex-warning-face
          font-lock-constant-face       ; \label, \ref, \cite arguments
          font-lock-function-name-face  ; \begin, \end
          font-lock-keyword-face
          font-lock-variable-name-face))

  ;; Belt and braces: skip every backslash-plus-letters sequence outright,
  ;; whatever face font-latex happened to give it.
  (setf (alist-get 'LaTeX-mode jinx-exclude-regexps)
        '("\\\\[a-zA-Z@]+"))

  (blackout 'jinx-mode)
  (global-jinx-mode 1))

(provide 'myspell)
;;; myspell.el ends here
