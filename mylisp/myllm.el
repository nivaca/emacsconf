;;; mylisp/myllm.el -*- lexical-binding: t; -*-

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) 
  :config
  (setq claude-code-ide-terminal-backend 'eat
        ;; Use regular window instead of side window
        claude-code-ide-use-side-window nil
        claude-code-ide-model "opus")
  (claude-code-ide-emacs-tools-setup)
  (defun nv-claude-code-ide-toggle-read-only ()
    "Toggle read-only/copy mode in the Claude buffer."
    (interactive)
    (if eat--semi-char-mode
        (progn
          (eat-emacs-mode)
          (setq-local eat-invisible-cursor-type '(box nil nil)))
      (eat-semi-char-mode)
      (setq-local eat-invisible-cursor-type nil))
    (eat--set-cursor nil :invisible))
  )



(provide 'myllm)
