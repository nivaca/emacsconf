;;; mylisp/mydired.el -*- lexical-binding: t; -*-

;; ================= dired ==================
(use-package dired
  :straight
  :defer t
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode)
              (all-the-icons-dired-mode)))  :bind ([S-f8] . dired)
  ("s-d" . dired)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-free-space nil)
  (dired-dwim-target t)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-filter-verbose nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (auto-revert-remote-files nil)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  ;; dired-omit-mode
  (dired-omit-verbose nil
                      dired-omit-files (concat "\\`[.]\\'")))


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
