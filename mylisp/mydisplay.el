;;; mylisp/mydisplay.el -*- lexical-binding: t; -*-

;; nivaca-pc: desktop casa
;; nivaca-dell: portátil dell
;; nivaca-xps: portátil dell xps 13
;; nivaca-tp: portátil Thinkpad X240



(use-package menu-bar
  :straight nil
  :bind
  ([S-f10] . menu-bar-mode)
  :config
  (menu-bar-mode -1)
  )


(use-package scroll-bar ;; tool bars etc.
  :straight nil
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
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



(use-package emacs  ;; various settings
  :config
  (setq visible-bell nil
        ring-bell-function 'ignore
        font-lock-maximum-decoration t
        truncate-partial-width-windows nil
        minibuffer-message-timeout 10
        column-number-mode t
        ;; pop-up-frames nil
        )

  ;; Don't resize emacs in steps, it looks weird.
  (setq window-resize-pixelwise t
        frame-resize-pixelwise t)

  ;; show trailing spaces
  (setq show-trailing-whitespace nil)

  ;; highlight current line
  (setq global-hl-line-mode -1)

  ;; Show me empty lines after buffer end
  (set-default 'indicate-empty-lines t)

  (global-display-line-numbers-mode t)

  ;; increase line space for better readability
  (setq-default line-spacing 3)

  ;; 1/4 of the total height
  ;; (setq max-mini-window-height 0.25)

  (when (display-graphic-p)
    (setq frame-title-format '(buffer-file-name "%f" ("%b")))
    (tooltip-mode -1)
    (blink-cursor-mode -1))

  )  ;; end: use-package emacs


;; ----------------------------------------------------------------------
;;                                Fonts
;; ----------------------------------------------------------------------
(use-package emacs  ;; fonts
  :config
  
  ;; (setq nv-frame-font "Hack ")  ;; mind the space
  (setq nv-frame-font "JetBrains Mono NL ")  ;; mind the space

  (when window-system
    (pcase (system-name)
      ;; PC escritorio casa
      ("nivaca-pc" (set-frame-font
                    (concat nv-frame-font "13")
                    nil t))
      ;; XPS 13
      ("nivaca-xps" (set-frame-font
                     (concat nv-frame-font "12")
                     nil t))
      ;; TP
      ("nivaca-tp" (set-frame-font
                    (concat nv-frame-font "12")
                    nil t))
      )
    )
  ;; Mac
  (when IS-MAC
    (set-frame-font (concat nv-frame-font " 18")))

  )


;; -----------------------------------------------------------------
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




;; ----------------------------------------------------------------------
;;                             cursor type
;; ----------------------------------------------------------------------
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(use-package emacs  ;; cursors
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

  ;; ----------------------------------------------------------------------
  ;;                          distinguish dashes
  ;; ----------------------------------------------------------------------
  ;; https://emacs.stackexchange.com/questions/9623/tell-a-dash-an-en-dash-and-an-emdash-apart
  ;;
  (let* (
         (glyph-en-dash (make-glyph-code ?\u002D 'font-lock-keyword-face))
         (glyph-em-dash (make-glyph-code ?\u002D 'font-lock-function-name-face)) )
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table 8211 `[,glyph-en-dash ,glyph-en-dash])
    (aset buffer-display-table 8212 `[,glyph-em-dash ,glyph-em-dash ,glyph-em-dash]))

  )


;; ----------------------------------------------------------------------
;;                              whitespace
;; ----------------------------------------------------------------------
(use-package whitespace
  :disabled
  :straight t
  :custom
  (whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
  (whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
   '(
     (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     (newline-mark 10 [182 10]) ; 10 LINE FEED
     (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
     ))
  :init (setq highlight-indent-guides-method 'character)
  :blackout whitespace-mode)


;; ----------------------------------------------------------------------
;;                                beacon
;; ----------------------------------------------------------------------
(use-package beacon
  ;; Highlight cursor position in buffer
  :straight t
  :custom
  (beacon-push-mark 10)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.3)
  (beacon-blink-duration 0.3)
  :init (beacon-mode 1)
  :blackout beacon-mode
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
  (global-set-key (kbd "C-x 3") 'nv-split-and-follow-vertically))



;; ----------------------------------------------------------------------
;;                  smart delete backward and forward
;; ----------------------------------------------------------------------
(straight-use-package
 '(nv-delete-forward
   :type git
   :host github
   :repo "nivaca/nv-delete-forward"
   :bind
   (("C-<delete>" . nv-delete-forward-all)
    ("M-<delete>" . nv-delete-forward))
   )
 )


(straight-use-package
 '(nv-delete-back
   :type git
   :host github
   :repo "nivaca/nv-delete-back"
   :bind
   (("C-<backspace>" . nv-delete-back-all)
    ("M-<backspace>" . nv-delete-back))
   )
 )



;; ----------------------------------------------------------------------
;;                             Centaur Tabs
;; ----------------------------------------------------------------------
(use-package centaur-tabs
  :straight t
  :hook (window-setup . centaur-tabs-mode)
  :config
  (setq centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-style "wave"
	centaur-tabs-cycle-scope 'tabs
	centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•")
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  ;; (centaur-tabs-mode t)
  )



;; ----------------------------------------------------------------------
;;                            All the icons
;; ---------------------------------------------------------------------
(use-package all-the-icons
  :straight t
  :defer)

(use-package all-the-icons-ivy
  :straight t
  :config
  (all-the-icons-ivy-setup)
  )

(use-package all-the-icons-dired
  :straight t
  :config
  )


;; Show color codes
;;-----------------------------------------------
(use-package rainbow-mode
  :straight t
  )




;; -----------------------------------------------
(use-package emacs  ;; buffers
  :config
  (defun nv-select-buffer-in-side-window (buffer alist)
    "Display buffer in a side window and select it"
    (let ((window (display-buffer-in-side-window buffer alist)))
      (select-window window)))

  (add-to-list 'display-buffer-alist '("\\*\\(?:Warnings\\|Compile-Log\\|Messages\\)\\*"
                                       (nv-select-buffer-in-side-window)
                                       (window-height . 0.20)
                                       (side . bottom)
                                       (slot . -5)
                                       (preserve-size . (nil . t))))
  )


;; ----------------------------------------------------------------------
(use-package prog-mode
  :straight
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


;; ====================================================================
(use-package popper
  :straight t
  :init
  (when window-system
    (pcase (system-name)
      ;; PC escritorio casa
      ("nivaca-pc" (bind-keys*
                    ("C-|"   . popper-toggle-latest)
                    ("M-|"   . popper-cycle)
                    ("C-M-|" . popper-toggle-type)))
      ;; XPS 13
      ("nivaca-xps" (bind-keys*
                     ("C-`"   . popper-toggle-latest)
                     ("M-`"   . popper-cycle)
                     ("C-M-`" . popper-toggle-type)))
      ;; TP
      ("nivaca-tp" (bind-keys*
                    ("C-|"   . popper-toggle-latest)
                    ("M-|"   . popper-cycle)
                    ("C-M-|" . popper-toggle-type))))
    ;; Mac
    (when IS-MAC (bind-keys*
                  ("C-|"   . popper-toggle-latest)
                  ("M-|"   . popper-cycle)
                  ("C-M-|" . popper-toggle-type))))
  ;;
  ;; (setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
        '(Custom-mode
          (compilation-mode . hide)
          messages-buffer-mode
          "^\\*Warnings\\*$"
          "^\\*straight-process\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*Matlab Help\\*"
          "^\\*Messages\\*$"
          "^\\*Backtrace\\*"
          "^\\*evil-registers\\*"
          "^\\*Apropos"
          "^Calc:"
          "^\\*TeX errors\\*"
          "^\\*ielm\\*"
          "^\\*TeX Help\\*"
          "\\*Shell Command Output\\*"
          "\\*Completions\\*"
          "\\*scratch\\*"
          "[Oo]utput\\*"))
  (popper-mode +1)
  )
;; --------------------------------------------------------------------



(use-package emacs  ;; popups
  :config
  ;; --------------------------------------------------------------------
  (setq display-buffer-base-action
        '(display-buffer-reuse-mode-window
          display-buffer-reuse-window
          display-buffer-same-window))

  ;; If a popup does happen, don't resize windows to be equal-sized
  (setq even-window-sizes nil)
  )


;; -------------------------------------------------------------------
;; Initial positions
(use-package emacs
  :config
  (defun nv-set-frame-position ()
    (interactive)
    (when window-system  ;; not in console
      (setq frame-resize-pixelwise t)
      (set-frame-position (selected-frame) 0 0)
      (pcase (system-name)
        ;; PC escritorio casa
        ("nivaca-pc" (set-frame-size (selected-frame) (/ 1920 2) 1080 t))
        ;; XPS 13
        ("nivaca-xps" (set-frame-size (selected-frame) (/ 1920 2) 1200 t))
        ;; TP
        ("nivaca-tp" (set-frame-size (selected-frame) (/ 1366 2) 768 t))
        ) ;; end: pcase
      ;; Mac
      (when IS-MAC
        (set-frame-size (selected-frame) 1024 600 t))
      ) ;; end: when window-system
    ) ;; end: defun nv-set-frame-position
  )




(provide 'mydisplay)
