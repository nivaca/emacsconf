;;; mylisp/myselect.el -*- lexical-binding: t; -*-


(use-package consult
  :straight t
  :demand t
  :bind (
         ;; ("C-s" . consult-line)
         ("C-M-j" . persp-switch-to-buffer*)
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
  ;; :disabled
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
  )


(use-package prescient
  :straight t
  :after selectrum
  :config
  (prescient-persist-mode)
  )


(use-package marginalia
  :straight t
  :after selectrum
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  )


(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless))
  )

(provide 'myselect)
