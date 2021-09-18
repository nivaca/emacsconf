;;; mylisp/mypackages.el -*- lexical-binding: t; -*-

;; package manager settings
;; ========================

;; Bootstrap straight.el
(defvar bootstrap-version)
(setq straight-base-dir (expand-file-name "~/.emacs.d"))
(let ((bootstrap-file (format "%s/%s" straight-base-dir "straight/repos/straight.el/bootstrap.el"))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; disable package.el
(setq package-enable-at-startup nil)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)




;; ===================================================================


(use-package blackout
  :straight t
  :config
  (blackout 'company)
  (blackout 'flycheck-mode)
  (blackout 'ivy)
  (blackout 'hungry-delete)
  (blackout 'gcmh-mode)
  (blackout 'outline-minor-mode)
  (blackout 'aggressive-indent-mode)
  (blackout 'eldoc-mode)
  (blackout 'hs-minor-mode)
  (blackout 'subword-mode)
  (blackout 'visual-line-mode)
  (blackout 'gcmh-mode)
  (blackout 'GCMH-mode)
  (blackout 'outline-mode)
  (blackout 'org-indent-mode)
  )


(provide 'mypackages)
