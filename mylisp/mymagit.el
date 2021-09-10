;;; mylisp/mymagit.el -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :defer t
  :bind ("C-c g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))


;; http://whattheemacsd.com/setup-magit.el-01.html
;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))



;; ========= Disable native GIT support ============
(setq vc-handled-backends nil)
(setq vc-handled-backends ())




(provide 'mymagit)
