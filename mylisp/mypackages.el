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


;; ------------------------------------------------------------
;; Blackout: the easy way to clean up your Emacs mode lighters
(use-package blackout
  :straight t)


(provide 'mypackages)
