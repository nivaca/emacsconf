;;; mylisp/myselect.el -*- lexical-binding: t; -*-


(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x m" . consult-bookmark)
         ("M-y" . consult-yank-from-kill-ring)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  ;; :config
  ;; (consult-preview-mode)
  )


;; (use-package vertico
;;   :disabled
;;   :straight t
;;   :init
;;   (vertico-mode)
;;   (setq vertico-resize nil)
;;   (setq vertico-cycle t)
;;   )


(use-package selectrum
  :straight t
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 7)
  ;; (selectrum-refine-candidates-function #'orderless-filter)
  ;; (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1)
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  )


;; (use-package prescient
;;   :straight t
;;   :after selectrum
;;   :config
;;   (prescient-persist-mode)
;;   )


(use-package marginalia
  :straight t
  :after selectrum
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  )


(use-package orderless
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))



(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'myselect)
