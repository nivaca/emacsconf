;;; mylisp/mydesktop.el -*- lexical-binding: t; -*-

;; perspective, savaplace, recentf

;; nivaca-pc: desktop casa
;; nivaca-dell: portátil dell
;; nivaca-xps: portátil dell xps 13
;; nivaca-tp: portátil Thinkpad X240


;; ======================================================================
;;                             dogears
;; ======================================================================
(straight-use-package
  '(dogears
    :type git
    :host github
    :repo "alphapapa/dogears.el"
    )
)

(use-package dogears
  :config
    (dogears-mode)
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
    ;; TP
    ("nivaca-tp" (setq custom-file (concat user-emacs-directory "tmp/tp/custom.el")))
    )
  ;; Mac oficina
  (when IS-MAC
    (setq custom-file (concat user-emacs-directory "tmp/mac/custom.el")))
  :hook
  (after-init . (lambda ()
                  (unless (file-exists-p custom-file)
                    (write-region "" nil custom-file))
                  (load custom-file))))




;; Remember mini-buffer history --------------------------
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
    ;; TP
    ("nivaca-tp" (setq savehist-file (concat user-emacs-directory "tmp/tp/savehist")))
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
    ;; TP
    ("nivaca-tp" (setq save-place-file (concat user-emacs-directory "tmp/tp/saved-places")))
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
    ;; PC escritorio casa
    ("nivaca-pc" (setq recentf-save-file (concat user-emacs-directory "tmp/pc/recentf")))
    ;; XPS 13
    ("nivaca-xps" (setq recentf-save-file (concat user-emacs-directory "tmp/xps/recentf")))
    ;; TP
    ("nivaca-tp" (setq recentf-save-file (concat user-emacs-directory "tmp/tp/recentf")))
    )
  ;; Mac oficina
  (when IS-MAC
    (setq recentf-save-file (concat user-emacs-directory "tmp/mac/recentf")))
  ;;
  (setq recentf-max-saved-items 300
        recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                          "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                          ".gz"
                          "~$" "/tmp/pc/" "/tmp/xps/" "/tmp/dell/" "/tmp/mac/" "/ssh:" "/sudo:" "/scp:")
        ;;recentf-auto-cleanup 600
        )
  (when (not noninteractive) (recentf-mode 1))

  (defun recentf-save-list ()
    "Save the recent list.
    Load the list from the file specified by `recentf-save-file',
    merge the changes of your current session, and save it back to
    the file."
    (interactive)
    (let ((instance-list (copy-sequence recentf-list)))
      (recentf-load-list)
      (recentf-merge-with-default-list instance-list)
      (recentf-write-list-to-file)))

  (defun recentf-merge-with-default-list (other-list)
    "Add all items from `other-list' to `recentf-list'."
    (dolist (oitem other-list)
      ;; add-to-list already checks for equal'ity
      (add-to-list 'recentf-list oitem)))

  (defun recentf-write-list-to-file ()
    "Write the recent files list to file.
    Uses `recentf-list' as the list and `recentf-save-file' as the
    file to write to."
    (condition-case error
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-file-coding-system recentf-save-file-coding-system)
          (insert (format recentf-save-file-header (current-time-string)))
          (recentf-dump-variable 'recentf-list recentf-max-saved-items)
          (recentf-dump-variable 'recentf-filter-changer-current)
          (insert "\n \n;;; Local Variables:\n"
                  (format ";;; coding: %s\n" recentf-save-file-coding-system)
                  ";;; End:\n")
          (write-file (expand-file-name recentf-save-file))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
      (error
       (warn "recentf mode: %s" (error-message-string error)))))
  )

(recentf-mode t)



;; ============== Disable autosave ===============
(setq auto-save-default nil)
(setq make-backup-files t)    ; don't make backup files


(provide 'mydesktop)
