;;; ============== mypython.el ==============

(use-package elpy
  :straight t
  :config
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       ;;elpy-module-pyvenv)
                       ))
  (setq py-python-command "/usr/local/bin/python3")
  (blackout 'elpy-mode "☕")
  (elpy-enable)
  )

;; (use-package lsp-pyright
;;   :straight t
;;   :hook
;;   (python-mode .
;;                (lambda ()
;;                  (require 'lsp-pyright)
;;                  (lsp-deferred))))  ; or lsp


(provide 'mypython)
