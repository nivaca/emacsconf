;;; mylisp/mydesktop.el -*- lexical-binding: t; -*-

;; nivaca-pc: desktop casa
;; nivaca-xps: portátil dell xps 13
;; n.vaughan20: portátil lenovo oficina

;; -------------------- Paths --------------------

(defvar nv-tmp-dir "/var/tmp/emacs")

(defvar nv-autosave-dir
  (expand-file-name "autosave/" nv-tmp-dir))

(defvar nv-backup-dir
  (expand-file-name "backup/" nv-tmp-dir))

(defvar nv-autosave-session-dir
  (expand-file-name "autosave/sessions/" nv-tmp-dir))

(dolist (dir (list nv-tmp-dir
                   nv-autosave-dir
                   nv-backup-dir
                   nv-autosave-session-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;; File hygiene ----------------------------------------------

(setopt
 backup-directory-alist
 `((".*" . ,nv-backup-dir)))

(setopt
 auto-save-file-name-transforms
 `((".*" ,nv-autosave-dir t)))

(setopt
 auto-save-list-file-prefix
 (expand-file-name ".saves-" nv-autosave-session-dir))


;;; ============ Elite minibuffer & completion polish ============

;; --- Safer autosaves (do NOT disable autosave) ---
(setopt
 auto-save-default t
 auto-save-timeout 20
 auto-save-interval 200)

;; --- Recursive minibuffers (major quality-of-life improvement) ---
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; --- Better Consult preview responsiveness ---
(setq consult-preview-key '(:debounce 0.3 any))

;; --- Orderless stability improvement ---
(setq completion-category-defaults nil)

;; --- Larger kill ring for consult-yank ---
(setq kill-ring-max 200)

;; --- Savehist smoother autosaving ---
(setq savehist-autosave-interval 300)

;; --- Recentf automatic maintenance ---
(setopt recentf-auto-cleanup 'mode)
(run-at-time 300 300 #'recentf-save-list)


;; --- Vertico directory editing feels natural ---
(use-package vertico-directory
  :straight (:type built-in)
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))


(defun nv--host-subdir ()
  "Return host-specific subdirectory name."
  (pcase (system-name)
    ("nivaca-pc" "pc")
    ("nivaca-xps" "xps")
    ("n.vaughan20" "lenovo")
    (_ "default")))

(defun nv--state-file (name)
  "Build a host-specific state file path."
  (expand-file-name
   (format "tmp/%s/%s" (nv--host-subdir) name)
   user-emacs-directory))

;; Ensure host tmp directory exists
(let ((dir (file-name-directory (nv--state-file "dummy"))))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; -------------------- Backups --------------------

(setopt
 backup-inhibited nil
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; -------------------- Custom file --------------------

(use-package cus-edit
  :straight (:type built-in)
  :init
  (setq custom-file (nv--state-file "custom.el"))
  :config
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file t))

;; -------------------- savehist --------------------

(use-package savehist
  :straight (:type built-in)
  :custom
  (history-length 1000)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  :init
  (setq savehist-file (nv--state-file "savehist"))
  (savehist-mode 1))

;; -------------------- saveplace --------------------

(use-package saveplace
  :straight (:type built-in)
  :init
  (setq save-place-file (nv--state-file "saved-places"))
  (save-place-mode 1))

;; -------------------- recentf --------------------

(use-package recentf
  :straight (:type built-in)
  :custom
  (recentf-max-menu-items 200)
  (recentf-max-saved-items 200)
  (recentf-exclude
   '("/auto-install/" ".recentf" "/repos/" "/elpa/"
     ".gz" "~$" "/ssh:" "/sudo:" "/scp:"))
  :init
  (setq recentf-save-file (nv--state-file "recentf"))
  (recentf-mode 1)
  :bind
  ("C-x m" . consult-recent-files))

(provide 'mydesktop)
