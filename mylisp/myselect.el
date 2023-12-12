;;; mylisp/myselect.el -*- lexical-binding: t; -*-

;; ==================== Consult ====================
;; Consult provides practical commands based on the Emacs completion
;; function completing-read. Completion allows you to quickly select an
;; item from a list of candidates.
(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x m" . consult-bookmark)
         ("M-y" . consult-yank-from-kill-ring)
         ("M-g i" . consult-imenu)
         ("C-x C-r" . consult-recent-file)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  ;; :config
  ;; (consult-preview-mode)
  )

;; -------------------- Vertico --------------------
;; Vertico provides a performant and minimalistic vertical completion
;; UI based on the default completion system.
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  (vertico-mouse-mode)
  )


;; -------------------- marginalia --------------------
;; This package provides marginalia-mode which adds
;; marginalia to the minibuffer completions. 
(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  )


;; ------------------------- Orderless -------------------------
;; Provides an orderless completion style that divides the pattern into
;; space-separated components, and matches candidates that match all of
;; the components in any order.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    :config
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  )


;; -------------------------- Embark ------------------------------
;; Emacs Mini-Buffer Actions Rooted in Keymaps.
;; This package provides a sort of right-click contextual menu for Emacs,
;; accessed through the embark-act command (which you should bind to a
;; convenient key), offering you relevant actions to use on a target
;; determined by the context:
(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )


(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )


;; ======================== affe ========================
;; This package provides an asynchronous fuzzy finder similar to
;; the fzf command-line fuzzy finder, written in pure Elisp. 
(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  )


(provide 'myselect)
