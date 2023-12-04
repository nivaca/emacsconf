;;; mylisp/mypackages.el -*- lexical-binding: t; -*-

;; package manager settings
;; ========================

(setq straight-base-dir (expand-file-name "~/.emacs.d/"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; ---------------------------------------------------

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
