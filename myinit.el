;;; myinit.el -*- lexical-binding: t; -*-

;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

;; Set some paths
(setq user-emacs-directory (expand-file-name "~/emacs/"))
(setq user-lisp-directory (expand-file-name "mylisp/" user-emacs-directory))

(add-to-list 'load-path user-lisp-directory)

;; Set eln-cache location
(when (featurep 'native-compile)
  (let ((eln-cache-dir (expand-file-name "~/.emacs/eln-cache")))
    ;; Only create or set if we have native compilation
    (setq native-comp-eln-load-path 
          (list eln-cache-dir 
                (car (last native-comp-eln-load-path))))))


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
                                        ; (use-package mypackages
                                        ; :straight nil)


;; =============== disable treesit ===============
(use-package treesit-auto
  :disabled t)

(with-eval-after-load 'treesit-auto
  (treesit-auto-mode -1)
  (setq treesit-auto-install nil))

(remove-hook 'after-init-hook #'global-treesit-auto-mode)

(with-eval-after-load 'treesit-auto
  (treesit-auto-mode -1)
  (setq treesit-auto-install nil))

;; Comprehensive treesit disabling
(setq treesit-extra-load-path nil)
(setq treesit-font-lock-level 0)
(setq treesit-load-name-override-list nil)
(setq major-mode-remap-alist nil)

;; Prevent automatic grammar installations
(advice-add 'treesit--install-language-grammar :around
            (lambda (orig-fun &rest args) nil))

;; Prevent treesit modes from being used (without cl-lib)
(setq major-mode-remap-alist
      (seq-filter (lambda (pair)
                    (not (string-match-p "treesit" (symbol-name (cdr pair)))))
                  (default-value 'major-mode-remap-alist)))

;; =================== ORG-mode ====================
                                        ; (require 'myorg)
(use-package myorg
  :straight nil)

;; ================= My editor settings ===================
                                        ; (require 'myedit)
(use-package myedit
  :straight nil)

;; =================== desktop etc. ====================
                                        ; (require 'mydesktop)
(use-package mydesktop
  :straight nil)

;; =============== Spell Checking ==================
                                        ; (require 'myspell)
(use-package myspell
  :straight nil)


;; =============== Flycheck ==================
;; (use-package flycheck
;;   ;; :straight t
;;   :diminish flycheck-mode
;;   :config
;;   (setq flycheck-global-modes nil)
;;   ;; (add-hook 'latex-mode-hook 'flycheck-mode)
;;   ;;(global-flycheck-mode)
;;   )


;; ================= AUCTEX =====================
                                        ; (require 'myauctex)
(use-package myauctex
  :straight nil)

;; =================  terminal ================
                                        ; (require 'myterm)
(use-package myterm
  :straight nil)

;; =================  Parentheses ================
                                        ; (require 'myparent)
(use-package myparent
  :straight nil)

;; ================ magit ===============
                                        ; (require 'mymagit)
(use-package mymagit
  :straight nil)

;; ===================== ediff ==========================
(setopt ediff-split-window-function 'split-window-horizontally)

;; ================= yasnippet ==================
                                        ; (require 'mycompletions)
(use-package mycompletions
  :straight nil)

;; ============== My minibuffer completion ===============
                                        ; (require 'myconsult)
(use-package myconsult
  :straight nil)

;; =================== minions ===================
;; mode manager
(use-package minions
  ;; :straight t
  :defer t
  :config
  (minions-mode 1)
  )


;; =================== helpful ===================
;; Helpful is an alternative to the built-in Emacs
;; help that provides much more contextual information.
(use-package helpful
  ;; :straight t
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

;; ================= hyperbole ===================
;; (require 'myhyp) ;; no por ahora...

;; ================= dired etc. ===================
                                        ; (require 'mydired)
(use-package mydired
  :straight nil)

;; ================= Projectile ===================
;; (require 'myprojectile) ;; no por ahora...

;; ================= lsp-mode ===================
;; (require 'mylsp)  ;; no por ahora..

;; ================= markdown ===================
                                        ; (require 'mymarkdown)
(use-package mymarkdown
  :straight nil)


;; ================= XML ===================
(require 'myxml)
(use-package myxml
  :straight nil)

;; ================= Python ===================
                                        ; (require 'mypython)
(use-package mypython
  :straight nil)

;; ================= pdf-tools ===================
;; (use-package pdf-tools
;;   ;; :straight t
;;   :defer t
;;   :commands (pdf-loader-install)
;;   :mode "\\.pdf\\'"
;;   :init
;;   (pdf-loader-install)
;;   :custom
;;   (pdf-annot-activate-created-annotations t)
;;   (pdf-view-continuous t)
;;   (pdf-view-display-size 'fit-width)
;;   (pdf-view-resize-factor 1.1)
;;   :config
;;   (add-to-list 'revert-without-query ".pdf")
;;   (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
;;   )


;; ===== Garbage Collector Magic Hack ====
(use-package gcmh
  :diminish
  ;; :straight t
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
                                        ; (require 'myfunctions)
(use-package myfunctions
  :straight nil)

;; ================= My aliases ===================
                                        ; (require 'myaliases)
(use-package myaliases
  :straight nil)

;; ================= epubs ==================
                                        ; (require 'myepub)
(use-package myepub
  :straight nil)

;; ================= server ==================
(require 'server)
(unless (server-running-p)
  (server-start))


;; =============== Dashboard ===============
(use-package dashboard
  ;; :if IS-LINUX
  ;; :disabled
  :init
  (add-hook 'after-init-hook 'dashboard-open)
  :config
  (setq dashboard-items '((bookmarks  . 10)
                          (recents . 7)
                          ;; (registers . 14)
                          ))

  ;; Header, footer, messages
  (setq dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-footer-messages '("")
        dashboard-startup-banner 1)
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
  :bind
  ("s-d" . dashboard-open)
  ;; :general
  ;; (general-define-key :keymaps 'dashboard-mode-map "e" nil)
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))


;; =====================================================
;; remove from mode line
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
                                        ; (require 'mydisplay)
(use-package mydisplay
  :straight nil)

;; ============= My themes configuration ==========
                                        ; (require 'mythemes)
(use-package mythemes
  :straight nil)

;; ================= KEY remap ===============
                                        ; (require 'mykeys)
(use-package mykeys
  :straight nil)

;; overwrite selected text
(delete-selection-mode 1)

;; ------------------------------------------------------------
(provide 'myinit)
;;; myinit.el ends here
