;;; myedit.el -*- lexical-binding: t; -*-

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; for all computers
(setq user-emacs-directory "~/emacs/")
(setq user-lisp-directory "~/emacs/mylisp/")

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path)
  )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))



;; ================== Initialization ==================
(load-file (expand-file-name "initialsetup.el" user-lisp-directory))



;; =============== Native compilation ===============
(when (not (version< emacs-version "28"))
  (message "Running Emacs 28+")
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors nil)
  )




;; =============== Package Management ===============
(load-file (expand-file-name "mypackages.el" user-lisp-directory))


;; ================= My editor settings ===================
(use-package myedit
  :straight
  :load-path user-lisp-directory
  )


;; =================== desktop etc. ====================
(use-package mydesktop
  :straight
  :load-path user-lisp-directory
  :config
  (recentf-cleanup)
  )


;; =============== Flyspell ==================
(use-package myspell
  :straight
  :load-path user-lisp-directory
  )


;; =============== Flycheck ==================
(use-package flycheck
  :straight t
  :defer y
  :diminish flycheck-mode
  :config
  (setq flycheck-global-modes nil)
  ;; (add-hook 'latex-mode-hook 'flycheck-mode)
  ;;(global-flycheck-mode)
  )


;; ================= AUCTEX =====================
(use-package myauctex
  :straight
  :load-path user-lisp-directory
  )



;; =================  Parentheses ================
(use-package myparent
  :straight
  :load-path user-lisp-directory
  )


;; ;; ===================== ansi-term =======================
;; (use-package vterm
;;   :straight t
;;   :custom
;;   (vterm-shell "fish")
;;   )

;; ================ magit ===============
(use-package mymagit
  :straight
  :load-path user-lisp-directory
  )


;; =================== ORG-mode ====================
(use-package myorg
  :straight
  :load-path user-lisp-directory
  )

;; ===================== ediff ==========================
(use-package ediff
  :straight nil
  :config (setq ediff-split-window-function 'split-window-horizontally))


;; ================= company ==================
(use-package mycompany
  :straight
  :if window-system
  :load-path user-lisp-directory
  )


;; ================= dired ==================
(use-package dired
  :straight
  :defer t
  :bind ([S-f8] . dired)
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  )

(use-package dired-narrow
  :straight t
  :defer t
  )

(use-package dired-subtree
  :straight t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
  )

;; no extra buffers when entering different dirs
(use-package dired-single
  :straight t
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
              (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
              (define-key dired-mode-map (kbd "^")
                (lambda ()
                  (interactive)
                  (dired-single-buffer "..")))))
  )


;; =================== minions ===================
;; mode manager
(use-package minions
  :straight t
  :defer t
  :config
  (minions-mode 1)
  )


;; =================== helpful ===================
;; Helpful is an alternative to the built-in Emacs
;; help that provides much more contextual information.
(use-package helpful
  :straight t
  :config
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol)
  )



;; ================= markdown ===================
(use-package mymarkdown
  :straight
  :disabled
  :load-path user-lisp-directory
  )


;; ================= treemacs ===================
(use-package mytreemacs
  :straight
  :load-path user-lisp-directory
  )


;; ===== Garbage Collector Magic Hack ====
(use-package gcmh
  :diminish
  :straight t
  :custom
  ;; Adopt a sneaky garbage collection strategy of waiting until idle time to
  ;; collect; staving off the collector while the user is working.
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-verbose nil)
  :init
  (gcmh-mode 1))


;; ============== My minibuffer completion ===============
(use-package myivy
  :straight
  :load-path user-lisp-directory
  )


;; ================= My functions ===================
(use-package myfunctions
  :straight
  :load-path user-lisp-directory
  )

;; ================= My aliases ===================
(use-package myaliases
  :straight
  :load-path user-lisp-directory
  )


;; ================= KEY remap ===============
(use-package mykeys
  :straight
  :load-path user-lisp-directory
  )

;; ================= server ==================
(use-package server 
  :config
  (unless (server-running-p)
    (server-start)))



;; ============= My themes configuration ==========
(use-package mythemes
  :straight
  :load-path user-lisp-directory
  )


;; ============= My display configuration ==========
(use-package mydisplay
  :straight
  :load-path user-lisp-directory
  )


;; -----------------------------------------
;; needed for ivy; goes at the end 
(setq ivy-initial-inputs-alist ())



;; =========================================
;; Startup time

;; (defun nv-display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;;            (format "%.2f seconds"
;;                    (float-time
;;                     (time-subtract after-init-time before-init-time)))
;;            gcs-done))

;; (add-hook 'emacs-startup-hook #'nv-display-startup-time)

(unless noninteractive
  (persp-state-load persp-state-default-file))


(nv-set-frame-position)




;; -------------------------------------------------------------------
(provide 'myinit)
;;; myinit.el ends here
