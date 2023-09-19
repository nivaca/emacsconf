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
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  ;; (define-key dired-mode-map [mouse-2] #'ignore)
  ;; (define-key dired-mode-map (kbd "<mouse-1>") nil)
  ;; (define-key dired-mode-map (kbd "<mouse-2>") nil)
  )


(use-package dired-narrow
  :straight t
  :defer t
  )


(use-package dired-subtree
  :straight t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
  )


;; no extra buffers when entering different dirs
(use-package dired-single
  :straight t
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


;; =====================================================================
;;                                 Neotree
;; =====================================================================
(use-package neotree
  :disabled t
  :straight (neotree :type git :host github :repo "jaypei/emacs-neotree")
  :bind ("<f5>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)

  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Every time when the neotree window is opened, let it find current file and jump to node
  (setq neo-smart-open t)

  ;; show hidden files
  (setq-default neo-show-hidden-files t)
)
(provide 'mydired)
