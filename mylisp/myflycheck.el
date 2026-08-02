;;; Mylisp/myflycheck.el -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t

  :hook
  (after-init . global-flycheck-mode)

  :custom

  ;; Don't check while typing every character
  (flycheck-check-syntax-automatically
   '(save idle-change mode-enabled))

  ;; Wait before rechecking
  (flycheck-idle-change-delay 0.4)

  ;; Highlight the whole line
  (flycheck-highlighting-mode 'lines)

  ;; Show errors in right fringe
  (flycheck-indication-mode 'right-fringe)

  ;; Automatically refresh after changing checkers
  (flycheck-display-errors-delay 0.25)

  ;; Emacs Lisp configuration
  (flycheck-emacs-lisp-load-path 'inherit)

  :bind
  (("C-c !" . flycheck-command-map)))



(use-package flycheck-posframe
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))


(provide 'myflycheck)
