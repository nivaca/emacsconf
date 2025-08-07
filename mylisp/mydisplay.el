;;; mylisp/mydisplay.el -*- lexical-binding: t; -*-
;; nivaca-pc: desktop casa
;; nivaca-dell: portátil dell
;; nivaca-xps: portátil dell xps 13

;; (message "»»»»»»»»»»»» Loading mydisplay.el ««««««««««««« ")

(use-package menu-bar
  :straight nil
  :config
  (menu-bar-mode 0)
  )


(use-package scroll-bar ;; tool bars etc.
  :straight nil
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode 1)
  )


(use-package tooltip
  :straight
  :defer t
  :custom
  (tooltip-mode -1))


(use-package time
  :straight
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t)
  )


;; ====================================================
(use-package emacs
  :config
  (if IS-LINUX
      (setenv "QT_QPA_PLATFORM" "wayland"))
  (setopt
   column-number-mode t
   display-line-numbers-type t  ;; también: 'relative
   even-window-sizes 'height-only
   fit-frame-to-buffer t
   fit-window-to-buffer-horizontally t
   font-lock-maximum-decoration t
   frame-inhibit-implied-resize t
   minibuffer-message-timeout 5
   pixel-scroll-precision-mode t
   ring-bell-function 'ignore
   split-height-threshold 80
   split-width-threshold 125
   suggest-key-bindings nil
   switch-to-buffer-in-dedicated-window 'pop
   switch-to-buffer-obey-display-actions t
   truncate-partial-width-windows nil
   visible-bell nil
   window-combination-resize t
   window-min-height 3
   window-min-width 30
   window-sides-vertical nil
   x-underline-at-descent-line nil
   )

  ;; Don't resize emacs in steps, it looks weird.
  (setopt window-resize-pixelwise t
          frame-resize-pixelwise t)

  ;; show trailing spaces
  (setopt show-trailing-whitespace nil)

  ;; highlight current line
  (setopt global-hl-line-mode nil)

  ;; Show me empty lines after buffer end
  (setopt indicate-empty-lines t)

  (global-display-line-numbers-mode t)

  ;; increase line space for better readability
  (setq-default line-spacing 3)

  ;; 1/4 of the total height
  ;; (setq max-mini-window-height 0.25)

  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode nil)
  (blink-cursor-mode -1)
  ;; transparency
  ;; (add-to-list 'default-frame-alist '(alpha-background . 90))

  (setq display-buffer-base-action
        '(display-buffer-reuse-mode-window
          display-buffer-reuse-window
          display-buffer-same-window))

  ;; If a popup does happen,
  ;; don't resize windows to be equal-sized
  (setopt even-window-sizes nil)
  
  )  ;; end: use-package emacs --------------------------



;; ------------------------------------------------------
;;                         Fonts
;; ------------------------------------------------------
;; fontaine ---------------------------------------------
(use-package fontaine
  :config
  (setq fontaine-presets
        '((pc
           :default-height 140
           :line-spacing 0.1)
          (xps
           :default-height 170
           :line-spacing 0.1)
          (mac
           :default-family "JetBrains Mono NL"
           :variable-pitch-family "Aptos"
           :default-height 180)
          (epub
           :default-family "JetBrains Mono NL"
           :variable-pitch-family "Times New Roman"
           :default-height 160)
          (t
           :default-family "JetBrains Mono NL"
           :fixed-pitch-serif-family "JetBrains Mono NL"
           :variable-pitch-family "JetBrains Mono NL"
           :default-weight Regular)))
  ;; select preset depending on system ------------------
  (pcase (system-name)
    ;; PC escritorio casa
    ("nivaca-pc"
     (fontaine-set-preset 'pc))
    ;; XPS 13
    ("nivaca-xps"
     (fontaine-set-preset 'xps))
    )
  ;; Mac
  (when IS-MAC
    (fontaine-set-preset 'mac)
    )
  ;; reset to default scaling
  (text-scale-adjust 0)
  )


;; -------------------------------------------------------
;; pretty-mode
(use-package pretty-mode
  :straight t
  :commands pretty-mode
  :config
  (hook-into-modes #'pretty-mode
                   '(emacs-lisp-mode-hook
                     coffee-mode-hook
                     python-mode-hook
                     ruby-mode-hook
                     haskell-mode-hook
                     latex-mode-hook
                     )))




;; ------------------------------------------------------------
;;                             cursor type
;; ------------------------------------------------------------
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(use-package emacs  ;; cursors etc.
  :config
  (defun nv-set-cursor-according-to-mode ()
    "change cursor color and type according to some minor modes."
    (cond
     (buffer-read-only
      (setq cursor-type 'hbar))
     (overwrite-mode
      (setq cursor-type 'hollow))
     (t
      (setq cursor-type '(bar . 2)))))
  (add-hook 'post-command-hook 'nv-set-cursor-according-to-mode)

  ;; --------------------------------------------
  ;; This is an optical wrap at the right margin
  (when (version<= "30" emacs-version)
    (global-visual-wrap-prefix-mode t))
  
  (global-visual-line-mode t)

  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow))

  (toggle-word-wrap)

  ;; --------------------------------------------
  (setq global-font-lock-mode 1) ; everything should use fonts
  (setq font-lock-maximum-decoration t)

  ;; Set frame title to file name
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; don't let the cursor go into minibuffer prompt
  (setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))


  ;; -----------------------------------------------------------
  ;;                          distinguish dashes
  ;; -----------------------------------------------------------
  ;; https://emacs.stackexchange.com/questions/9623/tell-a-dash-an-en-dash-and-an-emdash-apart
  ;;
  (let* (
         (glyph-en-dash (make-glyph-code ?\u002D 'font-lock-keyword-face))
         (glyph-em-dash (make-glyph-code ?\u002D 'font-lock-function-name-face)) )
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table 8211 `[,glyph-en-dash ,glyph-en-dash])
    (aset buffer-display-table 8212 `[,glyph-em-dash ,glyph-em-dash ,glyph-em-dash]))

  ) ;; end of use-package




;; -------------------------------------------------------------
;;                            whitespace
;; -------------------------------------------------------------
(use-package whitespace
  :straight t
  :custom
  (whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark))
  (whitespace-display-mappings
   '(
     (space-mark   ?\     [?\u00B7]     [?.])
     (space-mark   ?\xA0  [?\u00A4]     [?_])
     (newline-mark ?\n    [182 ?\n])
     (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])
     ))
  :init (setq highlight-indent-guides-method 'character)
  :blackout whitespace-mode)


;; -------------------------------------------------------------
;;                                pulsar
;; -------------------------------------------------------------
(use-package pulsar
  :straight (pulsar :type git :host gitlab :repo "protesilaos/pulsar")
  :custom
  (pulsar-pulse-functions ; Read the doc string for why not `setq'
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     delete-window
     delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     windmove-swap-states-right
     windmove-swap-states-left
     windmove-swap-states-up
     windmove-swap-states-down
     tab-new
     tab-close
     tab-next
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))
  :config
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-delay 0.1)
  )



;; ====================================================
(use-package emacs  ;; frame splits
  :config
  (defun nv-split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun nv-split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  (global-set-key (kbd "C-x 2") 'nv-split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'nv-split-and-follow-vertically)
  )



;; ---------------------------------------------------------------
;;                             Tabs
;; ---------------------------------------------------------------
;; (use-package emacs
;;   :custom
;;   (tab-line-new-button-show nil)  ;; do not show add-new button
;;   (tab-line-close-button-show nil)  ;; do not show close button
;;   (tab-line-separator " | ")  ;; delimitation between tabs
;;   :config
;;   (global-tab-line-mode t))



;; --------------------------------------------------------------
;;                        All the icons
;; --------------------------------------------------------------
(use-package all-the-icons
  :straight t
  :defer)

(use-package all-the-icons-dired
  :straight t
  :config
  )


;; Show color codes
;;-----------------------------------------------
(use-package rainbow-mode
  :straight t
  )

;; ----------------------------------------------------------------
(use-package prog-mode
  :straight ;; sic
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))


;; ----------------------------------------------------------------------
(use-package outline-minor-faces
  :straight t
  :defer t
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))



;; doom-modeline ---------------------------------
(use-package doom-modeline
  :disabled
  :straight t
  :init
  (doom-modeline-mode 1)
  )


;; indent-bars ------------------------------------
;; fast, configurable indentation guide-bars
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  ;; :hook ((python-mode yaml-mode) . indent-bars-mode)
  :config
  (setq
   indent-bars-prefer-character t
   ;; indent-bars-color '(highlight :face-bg t :blend 0.75)
   ;; indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   ;; indent-bars-unspecified-fg-color "white"
   ;; indent-bars-unspecified-bg-color "black"
   ))


;; vim-tar-bar -----------------------------------
(use-package vim-tab-bar
  :straight t
  :commands vim-tab-bar-mode
  :hook
  (after-init . vim-tab-bar-mode)
  :custom
  (vim-tab-bar-show-groups nil)
  :blackout)


(provide 'mydisplay)
