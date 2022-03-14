;;; mylisp/mydired.el -*- lexical-binding: t; -*-

;; ================= dired ==================
(use-package dired
  :straight
  :defer t
  :bind ([S-f8] . dired)
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
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

;; ====================== dirvish ===============================
;; A minimalistic yet versatile file manager based on Dired.
;; (straight-use-package 'dirvish)



;; =====================================================================
;;                                 Neotree
;; =====================================================================
(use-package neotree
  :straight (neotree :type git :host github :repo "jaypei/emacs-neotree")
  ;; :bind (("<f5>" . neotree-toggle))
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq-default neo-show-hidden-files t)
  (setq neo-window-width 35)
)


(provide 'mydired)
