;;; mylisp/myauctex.el -*- lexical-binding: t; -*-

(use-package tex
  :straight auctex
  :mode
  ("\\.tex\\'" . LaTeX-mode)
  :commands
  (LaTeX-mode TeX-mode)
  :init
  (setq-default TeX-master nil)
  (setq-default TeX-PDF-mode t)
  ;;
  (defun nv-setup-auctex ()
    "Sets AUCTeX up."
    (message "Setting up AUCTeX...")
    (jinx-mode)
    (outline-minor-mode)
    (TeX-fold-mode 1)
    (turn-on-reftex)

    (setq-local TeX-electric-math (cons "$" "$"))
    (setq-local TeX-electric-sub-and-superscript t)

    (LaTeX-add-environments
     '("adjustbox"
       (lambda (env)
         (let ((arg (read-string "Options (e.g. max width=\\textwidth): ")))
           (LaTeX-insert-environment env (concat "{" arg "}"))))))

    (add-to-list 'LaTeX-environment-list '("adjustbox"))

    (add-to-list 'LaTeX-indent-environment-list
                 '("adjustbox" LaTeX-indent-tabular))

    (TeX-add-symbols
     '("num" 1)
     '("si" 1)
     '("SI" 2)
     '("numrange" 2)
     '("SIrange" 3)
     '("latin" 1)
     '("enquote" 1)
     '("textlcsc" 1)))
  ;;
  :hook (LaTeX-mode . nv-setup-auctex)

  :custom
  (LaTeX-beamer-item-overlay-flag nil)
  (TeX-auto-local ".auctex-auto")
  (TeX-auto-save t)
  (TeX-auto-untabify t)
  (TeX-clean-confirm nil)
  (TeX-command-default "LatexMk")
  (TeX-complete-expert t)
  (TeX-insert-macro-default-style 'plain)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-show-compilation t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  ;; 
  (TeX-tree-roots
   (list
    (concat (string-trim
             (shell-command-to-string
              "kpsewhich -var-value=TEXMFROOT"))
            "/texmf-dist")
    "/opt/texlive/texmf-local"
    "~/texmf"))
  :config
  ;; ---------------- RefTeX ----------------
  (use-package reftex
    :straight nil
    :commands turn-on-reftex
    :config
    (reftex-plug-into-AUCTeX)
    :blackout reftex-mode)

  ;; ---------------- LatexMk ----------------
  (use-package auctex-latexmk
    :straight t
    :after tex
    :custom
    (auctex-latexmk-inherit-TeX-PDF-mode t)
    :config
    (auctex-latexmk-setup))
  )

;; ---------------- Viewer ----------------
(use-package emacs
  :after tex
  :config
  (with-eval-after-load 'tex
    (add-to-list 'TeX-view-program-list
                 '("okular" "okular --unique file:%o#src:%n%a"))
    (setcdr (assq 'output-pdf TeX-view-program-selection)
            '("okular"))))

;; ---------------- Font LaTeX tweaks ----------------
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
                        :slant 'normal
                        :inherit 'default))
  (setq font-latex-fontify-sectioning 1.0)
  (setq font-latex-fontify-script nil))

;; ---------------- Pretty mode ----------------
(with-eval-after-load 'pretty-mode
  (add-hook 'LaTeX-mode-hook #'pretty-mode-disable))

(provide 'myauctex)
