;;; mylisp/mydired.el -*- lexical-binding: t; -*-

;; ================= dired ==================
(use-package dired
  :straight
  :defer t
  :bind ([S-f8] . dired)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  ;; (mouse-1-click-follows-link 1.1)
  :init
  ;; (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode)
              (all-the-icons-dired-mode)))
  ;; (define-key dired-mode-map [mouse-2] #'ignore)
  ;; (define-key dired-mode-map (kbd "<mouse-1>") nil)
  ;; (define-key dired-mode-map (kbd "<mouse-2>") nil)
  :bind
  ("s-d" . dired)
  )

;; (use-package dired-narrow
;;   :straight t
;;   :defer t
;;   )

(use-package dired-quick-sort
  :straight t
  :after dired
  )


;; (use-package dired-subtree
;;   :straight t
;;   :after dired
;;   :config
;;   (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
;;   (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
;;   )


;; ;; no extra buffers when entering different dirs
(use-package dired-single
  :straight t
  :after dired
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
              (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
              (define-key dired-mode-map (kbd "^")
                          (lambda ()
                            (interactive)
                            (dired-single-buffer "..")))))
  )


;; ===================== dired+ =============================
(use-package dired+
  :disabled
  :straight t
  :after dired)


;; ===================== casual-dired =====================
(use-package casual-dired
  :disabled
  :straight
  (causal-dired
   :host github
   :repo "kickingvegas/casual-dired")
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))




(provide 'mydired)
