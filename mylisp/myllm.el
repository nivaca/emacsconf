;;; mylisp/myllm.el -*- lexical-binding: t; -*-

(add-hook 'find-file-hook #'vc-refresh-state)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'eat)
  :config
  ;; (claude-code-ide-emacs-tools-setup)
  )

(use-package ellama
  :disabled
  :straight t
  :init
  (require 'llm-gemini)
  :config
  (setq ellama-provider
        (make-llm-gemini
         :key "AIzaSyA4GIY0ywdDG5qsdLT0iv_Nx65LZrD91mQ"
         :chat-model "gemini-3-flash-preview"))
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setq ellama-keymap-prefix "C-c e")
  (setq llm-warn-on-nonfree nil)
  )


(provide 'myllm)
