;;; mylisp/myedit.el -*- lexical-binding: t; -*-

(use-package emacs
  :config
  ;; Don't highlight matches with jump-char - it's distracting
  (setq jump-char-lazy-highlight-face nil)

  ;; Easily navigate sillycased words
  (global-subword-mode 1)

  ;; Don't break lines for me, please
  (setq-default truncate-lines t)

  ;; Tabs
  (setq tab-width 2)
  (setq-default indent-tabs-mode nil)

  ;; Line spacing
  (setq-default line-spacing 3)

  ;; Standard indent
  (setq standard-indent 2)

  ;; Remove text in active region if inserting text
  (delete-selection-mode +1)

  ;; Never insert tabs
  (set-default 'indent-tabs-mode nil)

  ;; Remove trailing spaces
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; give keyboard focus to help window
  (setq-default help-window-select t)

  ;; context-menu-mode
  (when (not (version< emacs-version "28"))
    (context-menu-mode))
  
  )


;; =============== Scrolling ==================
;; Vertical Scroll
(use-package emacs
  :config
  (setq scroll-step 1)
  (setq scroll-margin 1)
  (setq scroll-conservatively 101)
  (setq scroll-up-aggressively 0.01)
  (setq scroll-down-aggressively 0.01)
  (setq auto-window-vscroll nil)
  (setq fast-but-imprecise-scrolling nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed t)
  ;; Horizontal Scroll
  (setq hscroll-step 1)
  (setq hscroll-margin 1)

  (when IS-MAC
    ;; sane trackpad/mouse scroll settings
    (setq mac-redisplay-dont-reset-vscroll t
          mac-mouse-wheel-smooth-scroll nil))
  )



;; ============ auto-indent ==============
(use-package auto-indent-mode
  :straight t
  :defer t
  :init
  :custom
  (auto-indent-on-save-file t)
  (auto-indent-delete-trailing-whitespace-on-save-file t)
  (auto-indent-untabify-on-save-file t)
  (auto-indent-indent-style 'aggressive)
  (auto-indent-key-for-end-of-line-then-newline "<M-return>")
  (auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")
  :config
  (auto-indent-global-mode)
  :blackout auto-indent-mode
  )



;; ================= Kill Ring ===================
(use-package popup-kill-ring
  :straight t
  :bind ("M-y" . popup-kill-ring))


;; ;; ===================== drag-stuff ====================
;; ;; use instead move-text (below)
;; (use-package drag-stuff
;;   :disabled
;;   :straight t
;;   :config
;;   (drag-stuff-global-mode 1)
;;   (drag-stuff-define-keys)
;;   ;; this is needed to forcefully diminish the mode
;;   (define-minor-mode drag-stuff-mode
;;     "Drag stuff around."
;;     :init-value nil
;;     :lighter ""
;;     :keymap drag-stuff-mode-map)
;;   )


;; ===================== move-text ====================
(use-package move-text
  :straight t
  :bind
  (("M-<up>"   . move-text-up)
   ("M-<down>" . move-text-down))
  :config
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
)


;; ===================== crux ====================
(use-package crux
  :straight t
  :bind
  (
   ("C-a" . crux-move-beginning-of-line)
   ("C-e" . move-end-of-line)
   ("<home>" . crux-move-beginning-of-line)
   ("<end>" . move-end-of-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   )
  )

;; =============== expand-region ==============
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region)
  :bind ("C-+" . er/expand-region)
  )


;; only change case when region active
;; ===================================================
(use-package emacs
  :config
  (defun ensure-region-active (func &rest args)
    (when (region-active-p)
      (apply func args)))

  (advice-add 'upcase-region :around 'ensure-region-active)
  (advice-add 'downcase-region :around 'ensure-region-active)
  )


(use-package goto-last-change
  :straight t
  :commands goto-last-change
  ;; S-f1
  )


;; Indicate minibuffer depth
(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode 1))


(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (select-enable-clipboard t "Use the clipboard")
  )

;; ============== unfill ==============
;; Functions providing the inverse of
;; Emacs' fill-paragraph and fill-region
(use-package unfill
  :straight t
  :defer t)

;; ======== whole-line-or-region =========
(use-package whole-line-or-region
  :straight t
  :config
  (whole-line-or-region-global-mode t)
  )



;; ============== iedit ==============
(use-package iedit
  :straight t
  :defer t
  )


;; ============== multiple-cursors ==============
(use-package multiple-cursors
  :straight t
  :defer t
  :bind
  (
   ("C-c m t" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)
   ;; ("M-C-<mouse-1>" . mc/add-cursor-on-click)
   )
  )


;; ============== bookmarks ==============
(use-package bookmark+
  :straight nil
  :load-path "otherlisp/bmkp"
  :custom
  (bookmark-version-control t)
  (auto-save-bookmarks t)
  (bookmark-save-flag 1)
  )


;; =========================================
;; Smart delete backward (à la oXygen XML)
(use-package nv-delete-back
  :straight (nv-delete-back :type git :host github :repo "nivaca/nv-delete-back")
  :bind
  (("C-<backspace>" . nv-delete-back-all)
   ("M-<backspace>" . nv-delete-back))
  )


;; =========================================
;; Smart delete forward (à la oXygen XML)
(use-package nv-delete-forward
  :straight (nv-delete-forward :type git :host github :repo "nivaca/nv-delete-forward")
  :bind
  (("C-<delete>" . nv-delete-forward-all)
   ("M-<delete>" . nv-delete-forward))
  )

;; (use-package hungry-delete
;;   :straight t
;;   :init
;;   (hungry-delete-mode)
;;   :bind
;;   (
;;    ("C-<backspace>" . hungry-delete-backward)
;;    ("C-<delete>" . hungry-delete-forward)
;;    )
;;   )

;; trim extraenus white-space
(use-package ws-butler
  :straight t
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'ws-butler-mode)
  (add-hook 'latex-mode-hook #'ws-butler-mode)
  )


;; ==================== AVY ====================
(use-package avy
  :defer t
  :straight
  :bind
  ("M-s" . avy-goto-char)
  ("M-j" . avy-goto-char-timer)
  ("M-g g" . avy-goto-line)
  )


;; ==============================================================
(use-package delsel
  :straight t
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit))
  )

;; ==============================================================
;; Read ePub files
(use-package nov
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )



;; ==============================================================
(use-package centered-cursor-mode
  :disabled t
  :straight t
  :config
  (global-centered-cursor-mode)
  :blackout
  )


;; ===================== ediff ==========================
(setq ediff-split-window-function 'split-window-horizontally)


(provide 'myedit)
