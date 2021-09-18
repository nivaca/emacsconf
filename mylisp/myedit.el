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
  )


;; winner mode
;; (use-package winner
;;   :if (not noninteractive)
;;   :straight t
;;   ;;:defer 5
;;   :bind (("s-r" . winner-redo)
;;          ("s-u" . winner-undo))
;;   :custom
;;   (winner-boring-buffers
;;    '("*Completions*"
;;      "*Compile-Log*"
;;      "*inferior-lisp*"
;;      "*Fuzzy Completions*"
;;      "*Apropos*"
;;      "*Help*"
;;      "*cvs*"
;;      "*Buffer List*"
;;      "*Ibuffer*"
;;      "*esh command on file*"))
;;   :config
;;   (winner-mode 1))



;; ;; ============ aggressive indent ==============
;; (use-package aggressive-indent
;;   :straight t
;;   :init
;;   (defun
;;       aggressive-indent-mode-on ()
;;     (interactive)
;;     (aggressive-indent-mode 1))
;;   :hook
;;   ((prog-mode) . aggressive-indent-mode-on)
;;   ;; turn it off for python
;;   ((python-mode) . aggressive-indent-mode)
;;   )



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


;; ===============  Yasnippet  ===============
(use-package yasnippet
  :functions yas-global-mode
  :blackout yas-minor-mode
  :defer 3
  :config
  (setq
   yas-verbosity 3
   yas-indent-line nil
   ;; This replaces the default dirs:
   ;; yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
   ;; This appends:
   yas-snippet-dirs (append yas-snippet-dirs
                            '("~/emacs/snippets"))
   )
  (yas-global-mode t)
  (yas-reload-all)
  )



;; ===================== drag-stuff ====================
(use-package drag-stuff
  :straight t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  ;; this is needed to forcefully diminish the mode
  (define-minor-mode drag-stuff-mode
    "Drag stuff around."
    :init-value nil
    :lighter ""
    :keymap drag-stuff-mode-map)
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




;; myundo -------------------------------------
;; code taken from emacs 28
(use-package myundo
  :straight
  :init
  (when (version< emacs-version "28.0")
    (defun undo-only (&optional arg)
      "Undo some previous changes.
Repeat this command to undo more changes. A numeric ARG serves as
a repeat count. Contrary to `undo', this will not redo a previous
undo."
      (interactive "*p")
      (let ((undo-no-redo t)) (undo arg)))

    (defun undo--last-change-was-undo-p (undo-list)
      (while (and (consp undo-list) (eq (car undo-list) nil))
        (setq undo-list (cdr undo-list)))
      (gethash undo-list undo-equiv-table))

    (defun undo-redo (&optional arg)
      "Undo the last ARG undos."
      (interactive "*p")
      (cond
       ((not (undo--last-change-was-undo-p buffer-undo-list))
        (user-error "No undo to undo"))
       (t
        (let* ((ul buffer-undo-list)
               (new-ul
                (let ((undo-in-progress t))
                  (while (and (consp ul) (eq (car ul) nil))
                    (setq ul (cdr ul)))
                  (primitive-undo arg ul)))
               (new-pul (undo--last-change-was-undo-p new-ul)))
          (message "Redo%s" (if undo-in-region " in region" ""))
          (setq this-command 'undo)
          (setq pending-undo-list new-pul)
          (setq buffer-undo-list new-ul))))))
  :bind
  ("C-z" . undo-only)
  ("C-S-z" . undo-redo)
  )


;; ================= avy ========================
(use-package avy
  :defer t
  :straight
  :bind
  ("M-s" . avy-goto-char)
  ("M-g g" . avy-goto-line)
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
(straight-use-package
 '(bookmark+
   :type git
   :host github
   :repo "emacsmirror/bookmark-plus"
   :custom
   (bookmark-version-control t)
   (auto-save-bookmarks t)
   (bookmark-save-flag 1)
   )
 )

;; =========================================
;; Smart delete backward (à la oXygen XML)
(straight-use-package
 '(nv-delete-back
   :type git
   :host github
   :repo "nivaca/nv-delete-back")
 )
(use-package nv-delete-back
  :bind
  (("C-<backspace>" . nv-delete-back-all)
   ("M-<backspace>" . nv-delete-back))
  )


;; =========================================
;; Smart delete forward (à la oXygen XML)
(straight-use-package
 '(nv-delete-forward
   :type git
   :host github
   :repo "nivaca/nv-delete-forward")
 )
(use-package nv-delete-forward
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

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  )
;; (global-set-key (quote [C-f5]) 'revert-buffer-no-confirm)


(use-package rg
  :straight t
  :defer t
  )


;; ======================================================================
(use-package delsel
  :straight t
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit))
  )


;; ======================================================================
(use-package embrace
  :straight t
  :bind
  ("C-," . embrace-commander)
  )



(provide 'myedit)
