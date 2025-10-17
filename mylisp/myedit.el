;;; mylisp/myedit.el -*- lexical-binding: t; -*-

(use-package emacs
  :straight
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

  ;; Indicate MB depth
  (minibuffer-depth-indicate-mode 1)

  ;; Never insert tabs
  (set-default 'indent-tabs-mode nil)

  ;; Remove trailing spaces
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; give keyboard focus to help window
  (setq-default help-window-select t)

  ;; context-menu-mode
  (when (not (version< emacs-version "28"))
    (context-menu-mode))

  ;; overwrite text
  (delete-selection-mode 1)

  ;; cliboard management
  ;; (selection-coding-system 'utf-8)
  ;; (select-enable-clipboard t "Use the clipboard")
  )


;; =============== Scrolling ==================
;; only for nivaca-xps and n.vaughan20
(use-package ultra-scroll
  :if (member system-name '("nivaca-xps" "n.vaughan20"))
  :straight (ultra-scroll
             :host github
             :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1)
  )

;; only for nivaca-pc
(use-package smooth-scroll
  :if (string= system-name "nivaca-pc")
  :straight t
  :config
  (smooth-scroll-mode t)
  :blackout
  )


;; ================= snap-indent =================
(use-package snap-indent
  :straight t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))



;; ================= Kill Ring ===================
(use-package popup-kill-ring
  :straight t
  :bind ("M-y" . popup-kill-ring))



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
  (("C-a" . crux-move-beginning-of-line)
   ("C-e" . move-end-of-line)
   ("<home>" . crux-move-beginning-of-line)
   ("<end>" . move-end-of-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("s-s" . crux-create-scratch-buffer)))


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
  ;; C-f1
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
   ))


;; ============== bookmarks ==============
(use-package bookmark+
  :straight
  (bookmark+ type: git :host github :repo "emacsmirror/bookmark-plus")
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
  :straight (nv-delete-forward :host github :repo "nivaca/nv-delete-forward")
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
  :disabled
  :straight (ws-butler :host github :repo "lewang/ws-butler")
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
(use-package wgrep
  :disabled t
  :straight t
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
  :disabled t
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )


;; ===================== ediff ==========================
(setq ediff-split-window-function 'split-window-horizontally)


;; ==================== speedrect ====================
;; Quick key bindings and other tools
;; for rectangle-mark-mode.
(use-package speedrect
  :straight (speedrect
             :type git
             :host github
             :repo "jdtsmith/speedrect"))

(provide 'myedit)
