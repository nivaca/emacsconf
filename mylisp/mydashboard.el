;;; mylisp/mydashboard.el -*- lexical-binding: t; -*-

(use-package dashboard
  ;; :disabled
  :init
  (add-hook 'after-init-hook 'dashboard-open)
  :config
  (setq dashboard-items '((bookmarks  . 5)
                          (recents . 5)
                          ;; (registers . 14)
                          ))

  ;; Header, footer, messages
  (setq dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-footer-messages '("")
        dashboard-startup-banner 'logo
        )
  ;; General config
  (setq dashboard-items-default-length 30
        dashboard-page-separator "\n\n"
        dashboard-set-file-icons nil
        dashboard-set-heading-icons nil
        dashboard-set-init-info nil
        dashboard-set-navigator t
        dashboard-week-agenda nil
        dashboard-center-content nil
        )
  (setq dashboard-path-style 'truncate-beginning
        dashboard-path-max-length 60
        dashboard-bookmarks-show-base 'nil
        dashboard-projects-show-base 'nil
        dashboard-recentf-show-base 'nil
        dashboard-recentf-item-format "%s  %s"
        dashboard-projects-item-format "%s  %s")
  (dashboard-setup-startup-hook)
  :bind
  ("s-d" . dashboard-open)
  ;; :general
  ;; (general-define-key :keymaps 'dashboard-mode-map "e" nil)
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(remove-hook 'window-size-change-functions #'dashboard-resize-on-hook)


(provide 'mydashboard)
