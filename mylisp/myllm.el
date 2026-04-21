;;; mylisp/myllm.el -*- lexical-binding: t; -*-

(add-hook 'find-file-hook #'vc-refresh-state)
(add-to-list 'vc-handled-backends 'Git)
(advice-add 'project-root :filter-return #'expand-file-name)

(use-package vterm
  :straight t)q

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  ;; :custom
  ;; (claude-code-ide-terminal-backend 'eat)
  ;; :config
  ;; (claude-code-ide-emacs-tools-setup)
  )


(provide 'myllm)
