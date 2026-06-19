;;; init.el -*- lexical-binding: t; -*-

;;; Code:

;; Set some paths
(setq user-emacs-directory (expand-file-name "~/emacs/"))
(setq user-lisp-directory (expand-file-name "mylisp/" user-emacs-directory))
(add-to-list 'load-path user-lisp-directory)


;; Set eln-cache location
(when (featurep 'native-compile)
  (let ((eln-cache-dir (expand-file-name "~/.emacs.d/eln-cache")))
    ;; Only create or set if we have native compilation
    (setq native-comp-eln-load-path 
          (list eln-cache-dir 
                (car (last native-comp-eln-load-path))))))


(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setopt exec-path (append exec-path '("/usr/local/bin")))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))


(setenv "XDG_CURRENT_DESKTOP" "KDE")
(setenv "QT_QPA_PLATFORMTHEME" "kde")

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

;; =============== Compile Angel ==============
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; =================== ORG-mode ====================
(require 'myorg)

;; ================= My editor settings ===================
(require 'myedit)
;; (load "myedit")

;; =================== desktop etc. ====================
(require 'mydesktop)

;; =============== Spell Checking ==================
(require 'myspell)

;; ================= AUCTEX =====================
(require 'myauctex)

;; =================  terminal ================
(require 'myterm)

;; =================  Parentheses ================
(require 'myparent)

;; ================ magit ===============
(require 'mymagit)

;; ===================== ediff ==========================
(setopt ediff-split-window-function 'split-window-horizontally)

;; ================= yasnippet ==================
(require 'mycompletions)

;; ============== My minibuffer completion ===============
(require 'myconsult)


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
(require 'mydired)

;; ================= Projectile ===================
;; (require 'myprojectile) ;; no por ahora...

;; ================= lsp-mode ===================
;; (require 'mylsp)  ;; no por ahora..

;; ================= markdown ===================
(require 'mymarkdown)

;; ================= XML ===================
(require 'myxml)


;; ==================== projects ====================
(add-hook 'find-file-hook #'vc-refresh-state)
(add-to-list 'vc-handled-backends 'Git)
(advice-add 'project-root :filter-return #'expand-file-name)


;; ================= Python ===================
;; (require 'mypython)

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
;; (use-package gcmh
;;   :diminish
;;   ;; :straight t
;;   :custom
;;   ;; Adopt a sneaky garbage collection strategy of waiting
;;   ;; until idle time to collect; staving off the collector
;;   ;; while the user is working.
;;   (gcmh-idle-delay 5)
;;   (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
;;   (gcmh-verbose nil)
;;   :init
;;   (gcmh-mode 1))

;; Garbage collection -----------------
;; https://jackjamison.xyz/blog/emacs-garbage-collection/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000000))
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 1.2 t 'garbage-collect)


;; ================= My functions ===================
(require 'myfunctions)

;; ================= My aliases ===================
(require 'myaliases)


;; ================= epubs ==================
(require 'myepub)

;; ================= LLMs ==================
(require 'myllm)

;; ================= server ==================
(require 'server)
(unless (server-running-p)
  (server-start))


;; =============== Dashboard ===============
(require 'mydashboard)


;; =============== Elfeed ===============
(use-package elfeed
  :disabled
  :straight t
  :defer t
  :config
  (load "~/emacs/elfeed-feeds.el"))


;; =================== helpful =======================
(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))


;; ============= buffer terminator ===============
(use-package buffer-terminator
  :straight t
  :custom
  (buffer-terminator-verbose t)
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
  :config
  (buffer-terminator-mode 1))



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
  (blackout 'buffer-terminator-mode)
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
;; (load "mydisplay")

;; ============= My themes configuration ==========
(require 'mythemes)
;; (load "mythemes")

;; ================= KEY remap ===============
(require 'mykeys)

;; -------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
