;;; mylisp/mydesktop.el -*- lexical-binding: t; -*-

;; nivaca-pc: desktop casa
;; nivaca-xps: portátil dell xps 13
;; n.vaughan20: portátil lenovo oficina


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


;; ============== Custom edit file ================
(use-package cus-edit
  :straight
  :init
  (pcase (system-name)
    ;; PC escritorio casa
    ("nivaca-pc" (setq custom-file (concat user-emacs-directory "tmp/pc/custom.el")))
    ;;
    ;;; XPS 13
    ("nivaca-xps" (setq custom-file (concat user-emacs-directory "tmp/xps/custom.el")))
    ;;
    ;;; lenovo
    ("n.vaughan20" (setq custom-file (concat user-emacs-directory "tmp/lenovo/custom.el")))
    )
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
    ;; lenovo
    ("n.vaughan20" (setq savehist-file (concat user-emacs-directory "tmp/lenovo/savehist")))
    )
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
    ;;
    ;;; XPS 13
    ("nivaca-xps" (setq save-place-file (concat user-emacs-directory "tmp/xps/saved-places")))
    ;;
    ;;; lenovo
    ("n.vaughan20" (setq save-place-file (concat user-emacs-directory "tmp/lenovo/saved-places")))
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
    ;;
    ;;; lenovo
    ("n.vaughan20" (setq recentf-save-file (concat user-emacs-directory "tmp/lenovo/recentf")))
    )
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
