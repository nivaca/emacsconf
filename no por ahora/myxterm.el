;;; mylisp/myxterm.el -*- lexical-binding: t; -*-

(global-company-mode 0)

(setq visible-bell nil
      ring-bell-function 'ignore
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      minibuffer-message-timeout 10
      )

;; do not show scroll bars
(scroll-bar-mode -1)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Line spacing
(setq-default line-spacing 5)

;; Standard indent
(setq standard-indent 2)


;; remove toolbar
(tool-bar-mode -1)


;; Do not remove menu bar
(menu-bar-mode -1)

;; turn off the annoying alarm bell
(setq ring-bell-function 'ignore)


;; --------------------------------------------
;; This is an optical wrap at the right margin
(global-visual-line-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set frame title to file name
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))



;; =================== diminish ======================
(use-package diminish
  :ensure t
  :config
  (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "smartparens" '(diminish 'smartparens-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "company" '(diminish 'company-mode))
  (eval-after-load "flycheck-mode" '(diminish 'flycheck-mode))
  (eval-after-load "ivy" '(diminish 'ivy-mode))
  (diminish 'python-mode)
  (diminish 'flyspell-mode)
  (diminish 'visual-line-mode)
  )


;; =============scratch-pop===============
(use-package scratch-pop
  :ensure t
  :defer t
  )

;; ============= whitespace =============
(use-package whitespace
  :ensure t
  :defer t
  :init (setq highlight-indent-guides-method 'character)
  :diminish whitespace-mode
  )


;; ========= highlight-indent-guides ===================
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :config
  (progn
    (setq highlight-indent-guides-character ?\|)
    (set-face-foreground 'highlight-indent-guides-character-face "lightgray")
    )
  )


;; ============ auto-indent ==============
(use-package auto-indent-mode
  :ensure t
  :defer t
  :init
  (setq auto-indent-on-save-file t
        auto-indent-delete-trailing-whitespace-on-save-file t
        auto-indent-untabify-on-save-file t
        auto-indent-indent-style 'aggressive
        auto-indent-key-for-end-of-line-then-newline "<M-return>"
        auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>"
        )
  ;; :config (auto-indent-global-mode)
  :diminish auto-indent-mode
  )

;; ---------------- Mouse support ------------------------
(require 'xt-mouse)
(xterm-mouse-mode)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

(setq mouse-wheel-follow-mouse 't)

(defvar alternating-scroll-down-next t)
(defvar alternating-scroll-up-next t)

(defun alternating-scroll-down-line ()
  (interactive "@")
    (when alternating-scroll-down-next
;      (run-hook-with-args 'window-scroll-functions )
      (scroll-down-line))
    (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

(defun alternating-scroll-up-line ()
  (interactive "@")
    (when alternating-scroll-up-next
;      (run-hook-with-args 'window-scroll-functions)
      (scroll-up-line))
    (setq alternating-scroll-up-next (not alternating-scroll-up-next)))

(global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)


;; ---------------------------------------1
(provide 'myxterm)
