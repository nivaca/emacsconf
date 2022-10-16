;;; myinit.el -*- lexical-binding: t; -*-

;;; Code:

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

(setq byte-compile-warnings '(cl-functions))

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


;; =================== ORG-mode ====================
(use-package myorg
  :straight
  :load-path user-lisp-directory
  )


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


;; ================ magit ===============
(use-package mymagit
  :straight
  :load-path user-lisp-directory
  )

;; ===================== ediff ==========================
(use-package ediff
  :straight nil
  :config (setq ediff-split-window-function 'split-window-horizontally))



;; ================= completions ==================
(use-package mycompletions
  :straight
  :load-path user-lisp-directory
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



;; ================= dired etc. ===================
(use-package mydired
  :straight
  :load-path user-lisp-directory
  )


;; ================= markdown ===================
(use-package mymarkdown
  :straight
  ;; :disabled
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
(use-package myselect
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


;; ============= My projectile ==========
(use-package myprojectile
  :disabled to
  :straight
  :load-path user-lisp-directory
  )

;; ============= My themes configuration ==========
(use-package mythemes
  :straight
  :load-path user-lisp-directory
  )

;; ============= My note-taking configuration ==========
(use-package mynotes
  :straight
  :load-path user-lisp-directory
  )


;; ============= My display configuration ==========
;; (use-package mydisplay
;;   :straight
;;   :load-path user-lisp-directory
;;   )
(load-file (concat user-emacs-directory "mylisp/mydisplay.el"))


;; =============== Dashboard ===============
(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-items '(
                          (bookmarks  . 5)
                          (recents . 15)
                          ;; (registers . 14)
                          ))

  ;; Header, footer, messages
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-footer-messages '(""))
  (setq dashboard-footer-icon
        (all-the-icons-octicon "zap"
                               :height 0.00001
                               :v-adjust -0.05
                               :face 'font-lock-keyword-face))
  (setq dashboard-startup-banner 'logo)
  ;; General config
  (setq dashboard-items-default-length 30
        dashboard-page-separator "\n\n"
        dashboard-set-file-icons nil
        dashboard-set-heading-icons nil
        dashboard-set-init-info nil
        dashboard-set-navigator t
        dashboard-week-agenda nil
        dashboard-center-content nil
        )
  (setq dashboard-path-style 'truncate-beginning
        dashboard-path-max-length 60
        dashboard-bookmarks-show-base 'nil
        dashboard-projects-show-base 'nil
        dashboard-recentf-show-base 'nil
        dashboard-recentf-item-format "%s  %s"
        dashboard-projects-item-format "%s  %s")
  (dashboard-setup-startup-hook)
  ;; :general
  ;; (general-define-key :keymaps 'dashboard-mode-map "e" nil)
  )


;; ===================================================================


(use-package emacs
  :config
  (blackout 'aggressive-indent-mode)
  ; (blackout 'company)
  (blackout 'eldoc-mode)
  (blackout 'flycheck-mode)
  (blackout 'gcmh-mode)
  (blackout 'GCMH-mode)
  (blackout 'hs-minor-mode)
  (blackout 'hungry-delete)
  ; (blackout 'ivy)
  (blackout 'org-indent-mode)
  (blackout 'outline-minor-mode)
  (blackout 'outline-mode)
  (blackout 'subword-mode)
  (blackout 'visual-line-mode)
)

;; =========================================
;; Startup time

;; (defun nv-display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;;            (format "%.2f seconds"
;;                    (float-time
;;                     (time-subtract after-init-time before-init-time)))
;;            gcs-done))

;; (add-hook 'emacs-startup-hook #'nv-display-startup-time)



;; Locate the frame correctly
(nv-set-frame-position)


;; -------------------------------------------------------------------
(provide 'myinit)
;;; myinit.el ends here
