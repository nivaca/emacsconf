;;; mylisp/myepub.el -*- lexical-binding: t; -*-

(defun nv-epub-setup ()
  (interactive)
  (setq visual-fill-column-center-text t
        mode-line-format nil
        nov-header-line-format ""
        cursor-type nil
        fontaine-set-preset 'epub
        display-line-numbers-mode nil)
  ;; (buffer-face-mode)
  display-line-numbers-mode nil)


(use-package nov
  :straight t
  :config
  (nv-epub-setup)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'nv-epub-setup))





(provide 'myepub)
