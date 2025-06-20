;;; mylisp/myorg.el -*- lexical-binding: t; -*-

(use-package org
  :straight t
  :hook (org-mode . nv-org-mode-setup)
  :bind (:map org-mode-map
              ;; ("<RET>" . nv-org-return-dwim)
              ("<C-S-left>" . nil)
              ("<C-down>" . nil)
              ("<C-up>" . nil)
              ("<M-S-down>" . nil)
              ("<M-S-left>" . nil)
              ("<M-S-right>" . nil)
              ("<M-S-up>" . nil)
              ("<M-down>" . nil)
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("<M-up>" . nil)
              ("<S-down>" . nil)
              ("<S-left>" . nil)
              ("<S-right>" . nil)
              ("<S-up>" . nil)
              ("C-<tab>" . nil)
              ("C-S-<tab>" . nil)
              )
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org-protocol)
  (setq org-directory (expand-file-name "~/roamnotes"))
  (setq org-adapt-indentation nil
        org-babel-default-header-args '((:eval . "never-export"))
        org-confirm-babel-evaluate t ;; nil
        org-cycle-separator-lines 2
        ;; org-link-descriptive t 
        org-edit-src-content-indentation 0
        org-export-with-smart-quotes t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-block-startup nil
        org-hide-emphasis-markers nil
        org-image-actual-width 300
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars nil
        org-pretty-entities t
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-folded 'showeverything
        org-support-shift-select t
        org-ellipsis " ⤵" ;; … ⤵ ▼ ⬎
        org-cycle-separator-lines 1
        org-catch-invisible-edits 'smart ;; 'show-and-error 
        )
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (python . t)
     ))
  )


(defun nv-org-mode-setup ()
  (org-indent-mode -1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq
   org-adapt-indentation nil
   org-hide-leading-stars nil
   org-odd-levels-only nil)
  ;; ----------------------------------------------------------
  ;; org faces
  (setq nv-frame-font "JetBrains Mono NL")
  (set-face-attribute 'org-document-title nil
                      :font nv-frame-font :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
                        :font nv-frame-font :weight 'medium :height (cdr face)))
  ;; (unbind-key "<tab>" org-mode-map)
  )



;; ========================================================
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-directory (expand-file-name "~/roamnotes"))
  (setq org-roam-db-location (expand-file-name "~/.emacs.d/org-roam.db"))
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("d" "default" plain
           (file "~/roamnotes/templates/defaulttemplate.org")
           :target (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
          ("c" "computing note" plain
           (file "~/roamnotes/templates/comptemplate.org")
           :target (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("r" "reading note" plain
           (file "~/roamnotes/templates/readtemplate.org")
           :target (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          )
        )
  (org-roam-setup)
  ;;
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  ;;
  (defun org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory)))
  ;;
  :bind
  (("<f7>" . org-roam-node-find)
   ("S-<f7>" . org-roam-rg-search)
   (:map org-mode-map
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n o" . org-id-get-create)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n l" . org-roam-buffer-toggle)))
  );; fin "use-package org-roam"



;; =========================================================
;; org-remark
(use-package org-remark
  :straight (org-remark :type git :host github :repo "nobiot/org-remark")
  :config
  (org-remark-create "green"
                     '(:background "green" :foreground "blue")
                     '(CATEGORY "important"))
  :after org
  )

;; ============================================================
;; org-modern (minad)
(use-package org-modern
  :disabled t
  :after org
  :config
  (global-org-modern-mode)
  )


;; ============================================================
;; org-ql
(use-package org-ql
  :straight (org-ql :host github :repo "alphapapa/org-ql")
  :after org
  )


;; ============================================================
;; toc-org
(use-package toc-org
  :straight t
  :after org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))


;; =============================================================
;; org-tree-side
(use-package org-side-tree
  :straight (org-side-tree
             :host github
             :repo "localauthor/org-side-tree")
  :after org) 

;; ============================================================
;; org-appear
(use-package org-appear
  :straight (org-appear
             :type git
             :host github
             :repo "awth13/org-appear")
  :after org
  :config
  (setopt
   org-hide-emphasis-markers t
   org-appear-autoemphasis t
   org-pretty-entities t
   org-appear-autoentities t
   org-appear-autokeywords t
   org-link-descriptive t ;; = org-descriptive-link (obsolete)
   org-appear-autolinks t
   org-appear-autosubmarkers t
   org-appear-delay 0
   org-appear-inside-latex nil
   org-appear-trigger 'always
   ;; org-hidden-keywords
   )
  :hook
  (org-mode . org-appear-mode)
  )


;; quickroam
;; https://github.com/meedstrom/quickroam
(use-package quickroam
  :straight (quickroam :type git :host github :repo "meedstrom/quickroam")
  :after org-roam
  :config
  (add-hook 'org-mode-hook #'quickroam-enable)
  )


(provide 'myorg)
;; myorg.el ends here
