;;; myinit.el -*- lexical-binding: t; -*-

;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

;; for all computers
(setq user-emacs-directory "~/emacs/")
(setq user-lisp-directory "~/emacs/mylisp/")

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path)
  )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setopt exec-path (append exec-path '("/usr/local/bin")))


;; ================== Initialization ==================
(require 'initialsetup)


;; =============== Native compilation ===============
(setq byte-compile-warnings '(cl-functions))
(setq native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil
      native-comp-async-jobs-number 7
      )


;; =============== Package Management ===============
(require 'mypackages)

;; =================== ORG-mode ====================
(require 'myorg)

;; ================= My editor settings ===================
(require 'myedit)

;; =================== desktop etc. ====================
(require 'mydesktop)

;; =============== Spell Checking ==================
(require 'myspell)

;; =============== Flycheck ==================
;; (use-package flycheck
;;   :straight t
;;   :diminish flycheck-mode
;;   :config
;;   (setq flycheck-global-modes nil)
;;   ;; (add-hook 'latex-mode-hook 'flycheck-mode)
;;   ;;(global-flycheck-mode)
;;   )


;; ================= AUCTEX =====================
(require 'myauctex)

;; =================  Parentheses ================
(require 'myparent)

;; ================ magit ===============
(require 'mymagit)

;; ===================== ediff ==========================
(setopt ediff-split-window-function 'split-window-horizontally)


;; ================= completions ==================
(require 'mycompletions)

;; ============== My minibuffer completion ===============
(require 'myselect)

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
  :bind
  (("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))  
  :config
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol)
)

;; ================= dired etc. ===================
(require 'mydired)

;; ================= markdown ===================
(require 'mymarkdown)


;; ================= pdf-tools ===================
(use-package pdf-tools
  :straight t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :init
  (pdf-loader-install)
  :config
  (add-to-list 'revert-without-query ".pdf")
  (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
  )


;; ===== Garbage Collector Magic Hack ====
(use-package gcmh
  :diminish
  :straight t
  :custom
  ;; Adopt a sneaky garbage collection strategy of waiting
  ;; until idle time to collect; staving off the collector
  ;; while the user is working.
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-verbose nil)
  :init
  (gcmh-mode 1))


;; ================= My functions ===================
(require 'myfunctions)

;; ================= My aliases ===================
(require 'myaliases)


;; ================= server ==================
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


;; =============== Dashboard ===============
(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-items '((bookmarks  . 5)
                          (recents . 15)
                          ;; (registers . 14)
                          ))

  ;; Header, footer, messages
  (setq dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-footer-messages '("")
        dashboard-startup-banner 'logo
        )
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

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))


;; =====================================================


(use-package emacs
  :config
  (blackout 'aggressive-indent-mode)
  (blackout 'eldoc-mode)
  (blackout 'flycheck-mode)
  (blackout 'gcmh-mode)
  (blackout 'GCMH-mode)
  (blackout 'hs-minor-mode)
  (blackout 'hungry-delete)
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



;; ============= My display configuration ==========
(require 'mydisplay)

;; ============= My themes configuration ==========
(require 'mythemes)

;; ================= KEY remap ===============
(require 'mykeys)

;; -------------------------------------------------------------------
(provide 'myinit)
;;; myinit.el ends here
