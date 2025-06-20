;;; mylisp/mydesktop.el -*- lexical-binding: t; -*-

;; perspective, savaplace, recentf

;; nivaca-pc: desktop casa
;; nivaca-xps: portátil dell xps 13

(defvar nv-tmp-dir "/var/tmp/emacs")

;; create tmp dir if it doesn't exist
(when (not (file-exists-p nv-tmp-dir))
  (make-directory nv-tmp-dir t))

(setopt
 auto-save-default nil
 backup-inhibited nil
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setopt backup-directory-alist `((".*" . ,nv-tmp-dir)))  ;; mind the comma!


;; ==================== easyseasion ====================
(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)
  :custom
  (easysession-mode-line-misc-info nil)
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes
  :init
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  :blackout easysession-save-mode
  )


;; ============== Custom edit file ================
(use-package cus-edit
  :straight
  :init
  (pcase (system-name)
    ;; PC escritorio casa
    ("nivaca-pc" (setq custom-file (concat user-emacs-directory "tmp/pc/custom.el")))
    ;; XPS 13
    ("nivaca-xps" (setq custom-file (concat user-emacs-directory "tmp/xps/custom.el")))
    )
  ;; Mac oficina
  (when IS-MAC
    (setq custom-file (concat user-emacs-directory "tmp/mac/custom.el")))
  :hook
  (after-init . (lambda ()
                  (unless (file-exists-p custom-file)
                    (write-region "" nil custom-file))
                  (load custom-file))))



;; =============== savehist ===============
;; Remember mini-buffer history 
(use-package savehist
  :straight
  :custom
  (history-length 1000)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :init
  (pcase (system-name)
    ;; PC escritorio casa
    ("nivaca-pc" (setq savehist-file (concat user-emacs-directory "tmp/pc/savehist")))
    ;; XPS 13
    ("nivaca-xps" (setq savehist-file (concat user-emacs-directory "tmp/xps/savehist")))
    )
  ;; Mac oficina
  (when IS-MAC
    (setq savehist-file (concat user-emacs-directory "tmp/mac/savehist")))
  :init
  (savehist-mode)
  )





;; =================== saveplace ===================
(use-package saveplace
  :straight t
  :config
  (save-place-mode)
  (pcase (system-name)
    ;; PC escritorio casa
    ("nivaca-pc" (setq save-place-file (concat user-emacs-directory "tmp/pc/saved-places")))
    ;; XPS 13
    ("nivaca-xps" (setq save-place-file (concat user-emacs-directory "tmp/xps/saved-places")))
    )
  ;; Mac oficina
  (when IS-MAC
    (setq save-place-file (concat user-emacs-directory "tmp/mac/saved-places"))
    )
  )



;; ============= recentf stuff ================
(use-package recentf
  :straight t
  :config
  (pcase (system-name)
    ;;
    ;;; PC escritorio casa
    ("nivaca-pc" (setq recentf-save-file (concat user-emacs-directory "tmp/pc/recentf")))
    ;;
    ;;; XPS 13
    ("nivaca-xps" (setq recentf-save-file (concat user-emacs-directory "tmp/xps/recentf")))
    )
  ;;
  ;;; Mac oficina
  (when IS-MAC
    (setq recentf-save-file (concat user-emacs-directory "tmp/mac/recentf")))
  ;;
  ;;
  (setopt recentf-max-menu-items 200
          recentf-max-saved-items 200
          recentf-exclude
          '("/auto-install/" ".recentf" "/repos/" "/elpa/"
            ".gz" "~$" "/tmp/pc/" "/tmp/xps/" "/tmp/dell/"
            "/tmp/mac/" "/ssh:" "/sudo:" "/scp:")
          )
  ;;
  (recentf-mode t)
  ;;
  :bind ("C-x m" . consult-recent-file)
  )

(provide 'mydesktop)
