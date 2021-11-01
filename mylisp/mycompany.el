;;; mylisp/mycompany.el -*- lexical-binding: t; -*-

(use-package company
  :straight
  :blackout
  :config
  (global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-limit 0)
  (company-begin-commands '(self-insert-command))
  (company-transformers '(company-sort-by-occurrence))
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  ;; (company-dabbrev-downcase nil)
  )

;; (add-hook 'after-init-hook 'global-company-mode)

(use-package company-auctex
  :straight t
  :defer t
  :config
  (company-auctex-init)
  (add-to-list 'company-backends '(company-auctex))
)

(with-eval-after-load 'company
  (add-hook 'latex-mode-hook 'company-mode)
  (add-hook 'lisp-mode-hook 'company-mode)
  )

(provide 'mycompany)
;;; mycompany.el ends here
