;;; mylisp/myllm.el -*- lexical-binding: t; -*-
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-cli-path "/home/nivaca/.local/bin/claude"
        claude-code-ide-terminal-backend 'eat))


(provide 'myllm)
