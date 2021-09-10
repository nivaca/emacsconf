;;; mylisp/myorg.el -*- lexical-binding: t; -*-

(use-package org
  :straight t
  :ensure
  :hook (org-mode . nv-org-mode-setup)
  :bind (:map org-mode-map
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
    (setq org-ellipsis " ▾"
        org-adapt-indentation nil
        org-confirm-babel-evaluate nil
        org-cycle-separator-lines 2
        org-edit-src-content-indentation 2
        org-export-with-smart-quotes t
        org-fontify-quote-and-verse-blocks t
        org-hide-block-startup nil
        org-hide-emphasis-markers t
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars nil
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-folded nil
        org-support-shift-select t
        org-hide-emphasis-markers nil
        )
  )


(defun nv-org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  ;; (unbind-key "<tab>" org-mode-map)
  )


;; ======================================================================
(use-package ox-slimhtml
  :straight t
  :after org
  )


;; ======================================================================
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/roamnotes")
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ;;
     ("f" "fleeting note" plain "#+filetags: :fleeting:\n\n%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ;;
     ("l" "literature note" plain
      (file "~/roamnotes/templates/booktemplate.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ;;
     ("c" "computing note" plain
      (file "~/roamnotes/templates/comptemplate.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     )
   )
  (org-roam-setup)
  :bind
  (("<f7>" . org-roam-node-find)
   (:map org-mode-map
         (("C-c n i" . org-roam-node-insert)
          ("C-c n o" . org-id-get-create)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n l" . org-roam-buffer-toggle)))
   )
  )


;; ======================================================================
;; Deft
(use-package deft
  :straight t
  :commands (deft)
  :custom
  (deft-directory "~/roamnotes")
  (deft-recursive t)
  (deft-extensions '("md" "org"))
  (deft-ignore-file-regexp ".+template.+")
  (deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (deft-use-filename-as-title t)
  )



(provide 'myorg)
