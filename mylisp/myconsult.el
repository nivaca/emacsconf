;;; mylisp/myconsult.el -*- lexical-binding: t; -*-

;; ==================== Consult ====================
;; Consult provides practical commands based on the Emacs completion function completing-read. Completion allows you to quickly select an item from a list of candidates.
(use-package consult
  :straight t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-x m" . consult-bookmark)
   ("M-y" . consult-yank-from-kill-ring)
   ("M-o" . consult-imenu)
   ("C-o" . consult-outline)
   ("C-x C-r" . consult-recent-file)
   :map minibuffer-local-map
   ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  ;; :config
  ;; (consult-preview-mode)
  )

(use-package consult-lsp
  :disabled 
  :straight t)

;; consult-org-roam
(use-package consult-org-roam
  :straight t
  :after org-roam
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  ;; (require 'consult-org-roam)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")
  :bind
  ;; ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))


;; -------------------- Vertico --------------------
;; Vertico provides a performant and minimalistic vertical completion UI based on the default completion system.
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  (vertico-mouse-mode))


;; ------------------- consult-dir ---------------------
;; Allows you to easily insert directory paths into the minibuffer prompt in Emacs.
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


;; -------------------- marginalia --------------------
;; This package provides marginalia-mode which adds marginalia to the minibuffer completions. 
(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light nil))
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))


;; ------------------ Orderless ------------------
;; Provides an orderless completion style.
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; ---------------------- Embark -------------------------
;; Emacs Mini-Buffer Actions Rooted in Keymaps. This package provides a sort of right-click contextual menu for Emacs, accessed through the embark-act command (which you should bind to a convenient key), offering you relevant actions to use on a target determined by the context:
(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; =============== embark-consult ===============
(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )




;; =============== consult-notes ===============
(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam 
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (consult-notes-org-roam-mode)
  (setq consult-notes-file-dir-sources
        '(("Org" ?o "~/roamnotes/")))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files)))

(provide 'myconsult)
