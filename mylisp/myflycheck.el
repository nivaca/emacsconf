;;; Mylisp/myflycheck.el -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :bind ("C-c !" . flycheck-command-map)
  :custom
  (flycheck-check-syntax-automatically
   '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.4)
  (flycheck-display-errors-delay 0.25)
  (flycheck-highlighting-mode 'lines)
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (when (fboundp 'global-flycheck-eglot-mode)
    (global-flycheck-eglot-mode 1)))


(use-package flycheck-posframe
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))


(provide 'myflycheck)
;;; myflycheck.el ends here
