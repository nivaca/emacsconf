;; ================ desktop save ===============
(require 'desktop)


(pcase (system-name)
  ;; PC escritorio casa
  ("nivaca-pc" (setq desktop-dirname (concat user-emacs-directory "tmp/pc/")))
  ;; XPS 13
  ("nivaca-xps" (setq desktop-dirname (concat user-emacs-directory "tmp/xps/")))
  )
;; Mac oficina
(when IS-MAC
  (setq desktop-dirname (concat user-emacs-directory "tmp/mac/")))

(setq desktop-base-file-name      "desktop"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)

(desktop-save-mode t)

(setq my-lock-file (concat desktop-dirname desktop-base-lock-name))

;; check if lock-file for desktop file exists
;; and accordingly deletes it
(if (file-exists-p my-lock-file)
    (progn
      (message "Deleting destop lock file...")
      (delete-file my-lock-file)))


;; (bind-key* "<C-kp-add>" 'desktop-save-mode nil)


(defun nv-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      ;;(message "Saving desktop file...")
      (setq temp nil)
    ;; (desktop-save desktop-path)))
    (desktop-save desktop-dirname)))


(add-hook 'auto-save-hook 'nv-desktop-save)