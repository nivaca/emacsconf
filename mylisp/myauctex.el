;;; mylisp/myauctex.el -*- lexical-binding: t; -*-

(use-package auctex
  :straight t
  :defer t
  :mode
  ("\\.tex\\'" . latex-mode)
  ("\\.ltx\\'" . latex-mode)
  :commands
  (latex-mode
   LaTeX-mode
   TeX-mode)
  ;; :hook
  ;; ((Tex-latex-mode . LaTeX-preview-setup)
  ;;  (Tex-latex-mode . flyspell-mode)      
  ;;  (Tex-latex-mode . turn-on-reftex))
  :init
  (setq-default TeX-master nil)
  :config
  (add-hook 'Tex-latex-hook 'LaTeX-preview-setup)
  (add-hook 'Tex-latex-hook 'flyspell-mode)
  (add-hook 'Tex-latex-hook 'turn-on-reftex)
  (add-hook 'Tex-latex-hook 'outline-minor-mode)
  (add-hook 'Tex-latex-hook 'hs-minor-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-PDF-mode t)
  (LaTeX-beamer-item-overlay-flag nil)
  (TeX-PDF-mode t)
  (TeX-quote-after-quote nil)
  (TeX-open-quote "\"")
  (TeX-close-quote "\"")
  )



;; --------------------------------------------------------------------
(use-package reftex
  :after auctex
  :blackout reftex-mode)





;; other auctex settings ---------------------------------------------
(use-package emacs
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-to-list 'TeX-command-list
                           '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                             :help "Run latexmk on file"))))


  (with-eval-after-load "tex"
    (add-to-list 'TeX-view-program-list '("okular" "/usr/bin/okular %o"))
    (setcdr (assq 'output-pdf TeX-view-program-selection) '("okular")))


  (defun TeX-insert-quote ()
    " "
    )



  (defun LaTeX-indent-item ()
    "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
    (save-match-data
      (let* ((offset LaTeX-indent-level)
             (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                              LaTeX-indent-level-item-continuation)
                         (* 2 LaTeX-indent-level)))
             (re-beg "\\\\begin{")
             (re-end "\\\\end{")
             (re-env "\\(itemize\\|\\enumerate\\|description\\)")
             (indent (save-excursion
                       (when (looking-at (concat re-beg re-env "}"))
                         (end-of-line))
                       (LaTeX-find-matching-begin)
                       (current-column))))
        (cond ((looking-at (concat re-beg re-env "}"))
               (or (save-excursion
                     (beginning-of-line)
                     (ignore-errors
                       (LaTeX-find-matching-begin)
                       (+ (current-column)
                          (if (looking-at (concat re-beg re-env "}"))
                              contin
                            offset))))
                   indent))
              ((looking-at (concat re-end re-env "}"))
               indent)
              ((looking-at "\\\\item")
               (+ offset indent))
              (t
               (+ contin indent))))))

  (defcustom LaTeX-indent-level-item-continuation 4
    "*Indentation of continuation lines for items in itemize-like
environments."
    :group 'LaTeX-indentation
    :type 'integer)

  (eval-after-load "latex"
    '(setq LaTeX-indent-environment-list
           (nconc '(("itemize" LaTeX-indent-item)
                    ("enumerate" LaTeX-indent-item)
                    ("description" LaTeX-indent-item))
                  LaTeX-indent-environment-list)))
  )



(provide 'myauctex)
