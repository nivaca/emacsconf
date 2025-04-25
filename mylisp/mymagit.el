;;; mylisp/mymagit.el -*- lexical-binding: t; -*-

;; (message "»»»»»»»»»»»» Loading mymagit.el ««««««««««««« ")


(use-package magit
  :straight t
  :defer t
  :commands (magit-status)
  :bind ("C-c g" . magit-status)
  :init
  ;; Close popup when commiting - this stops the commit window
  ;; hanging around
  (define-advice git-commit-commit (:after (&rest _))
    (delete-window))
  (define-advice git-commit-abort (:after (&rest _))
    (delete-window))
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  )


(use-package emacs
  :config
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  ;; full screen magit-status
  (define-advice magit-status (:around (orig-fun &rest args))
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fun args)
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  ;; ========= Disable native GIT support ============
  (setq vc-handled-backends nil)
  (setq vc-handled-backends ())
  )




;; ================== difftastic ==================
(use-package difftastic
  :straight t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(provide 'mymagit)
