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
  (menu-bar-mode 1)
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
        fit-window-to-buffer-horizontally t
        fit-frame-to-buffer t
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
  
  ;; (setq nv-frame-font "Iosevka Fixed ")  ;; mind the space
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
                     nil t))      )
    )
  ;; Mac
  (when IS-MAC
    (set-frame-font (concat nv-frame-font "18")
                    nil t))

  ;; Mono and Variable
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono NL" :weight 'regular :height 1.0)
  ;; (set-face-attribute 'fixed-pitch nil :font "Iosevka Fixed" :weight 'regular :height 1.0)
  ;; (set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160)
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
;;                                pulsar
;; ----------------------------------------------------------------------
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

;; ----------------------------------------------------------------------
;;                                Googles
;; ----------------------------------------------------------------------
;; Goggles highlights the modified region using pulse.
;; Currently the commands undo, yank, kill and delete are supported.
(use-package goggles
  :straight (goggles :type git :host github :repo "minad/goggles")
  :init
  (add-hook 'prog-mode-hook #'goggles-mode)
  (add-hook 'latex-mode-hook #'goggles-mode)
  (add-hook 'LaTeX-mode-hook #'goggles-mode)
  (add-hook 'org-mode-hook #'goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  (blackout 'goggles-mode " GG")
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



;; ---------------------------------------------------------------
;;                             Tabs
;; ---------------------------------------------------------------
(use-package emacs
  :custom
  (tab-bar-show t)
  :config
  (tab-bar-mode t)
  )




;; ----------------------------------------------------------------------
;;                            All the icons
;; ---------------------------------------------------------------------
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


;; ------------------------------------------------------------------
(use-package pdf-tools
  :disabled t
  :straight t
  :defer t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :commands (pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq-default pdf-view-continuous nil)
  (setq-default pdf-annot-activate-created-annotations t)
  ;; (require 'pdf-occur)
  )


;; -------------------------------------------------------------------
;; Initial position of main frame
(use-package emacs
  :config
  (defun nv-set-frame-position ()
    (interactive)
    (when window-system  ;; not in console
      (set-frame-position (selected-frame) 0 0)
      (pcase (system-name)
        ;; PC escritorio casa
        ("nivaca-pc"
         (defcustom nv-screen-size '(1920 1080)
           "Screen size in pixels"))
        ;; XPS 13
        ("nivaca-xps"
         (defcustom nv-screen-size '(1920 1200)
           "Screen size in pixels"))
        ;; TP
        ("nivaca-tp"
         (defcustom nv-screen-size '(1920 1200)
           "Screen size in pixels"))
        ) ;; end: pcase
      ;; Mac
      (when IS-MAC
        (defcustom nv-screen-size '(1920 1200)
          "Screen size in pixels"
          )
        )
      (setq frame-resize-pixelwise t)
      (set-frame-position (selected-frame) 0 0)
      (let ((dx -2)  ;; added to adjust width
            (dy +2)) ;; added to adjust height
        (set-frame-width
         (selected-frame) 
         (+
          (/
           (/ (nth 0 nv-screen-size) 2)  ;; half of screen width
           (frame-char-width))
          dx)
         nil)
        (set-frame-height
         (selected-frame)
         (+
          (/
           (nth 1 nv-screen-size)
           (frame-char-height))
          dy)
         nil)
        ) ; let
      ) ; when
    ) ; defun
  ;; call the function now:
  ;; (nv-set-frame-position)
  )

(provide 'mydisplay)
