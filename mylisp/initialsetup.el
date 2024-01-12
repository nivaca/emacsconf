;;; mylisp/initialsetup.el -*- lexical-binding: t; -*-

;; ==========================================================
;;; Turn off the annoying crap immediately
(setopt auto-save-default t
        backup-inhibited t
        dabbrev-case-distinction nil
        dabbrev-case-fold-search nil
        disabled-command-function nil
        echo-keystrokes 0.1
        global-auto-revert-mode t  ;; auto load changed files
        global-auto-revert-non-file-buffers t  ;; revert dired and other buffers
        large-file-warning-threshold 536870911
        load-prefer-newer t
        use-dialog-box nil  ;; don't pop up UI dialogs when prompting
        vc-follow-symlinks t  ;; Follow symbolic links
        )

;; Maximum number of lines to keep in the message log buffer.
(setopt message-log-max 8192)

;; Reduce debug output, well, unless we've asked for it.
(setopt debug-on-error nil
        jka-compr-verbose nil)

;; ==================== UTF-8 =======================
;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setopt locale-coding-system 'utf-8)
(setopt selection-coding-system 'utf-8)


;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setopt ad-redefinition-action 'accept)


;; Make apropos omnipotent. It's more useful this way.
(setopt apropos-do-all t)


;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setopt auto-mode-case-fold nil)


;; Less noise at startup. The dashboard/empty scratch buffer is good enough.
(setopt inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'lisp-mode
        initial-scratch-message nil)

(setq-default initial-major-mode 'emacs-lisp-mode)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session, where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))


;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setopt idle-update-delay 1.0)


;;; Minibuffer -----------------------------------------------------------------

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; _while_ we're in the minibuffer.
(setopt enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setopt echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setopt resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setopt mode-line-default-help-echo nil
      show-help-function nil)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setopt minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Don't display messages in the minibuffer when using the minibuffer
(defmacro nv-silence-motion-key (command key)
  (let ((key-command (intern (format "doom/silent-%s" command))))
    `(progn
       (defun ,key-command ()
         (interactive)
         (ignore-errors (call-interactively ',command)))
       (define-key minibuffer-local-map (kbd ,key) #',key-command))))
(nv-silence-motion-key backward-delete-char "<backspace>")
(nv-silence-motion-key delete-char "<delete>")

(setopt message-log-max t)


;; ---------------------------------------------------------------------------------

;; Share clipboard with system
(setopt select-enable-clipboard t)


;; Remove command line options that aren't relevant to our current OS; that
;; means less to process at startup.
(unless IS-MAC   (setopt command-line-ns-option-alist nil))
(unless IS-LINUX (setopt command-line-x-option-alist nil))



;;
;;; Optimizations

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)


;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)


;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setopt fast-but-imprecise-scrolling t)


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setopt frame-inhibit-implied-resize t)


;; Don't ping things that look like domain names.
(setopt ffap-machine-p-known 'reject)


;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a notable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)


;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setopt command-line-ns-option-alist nil))
(unless IS-LINUX (setopt command-line-x-option-alist nil))


;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setopt delete-by-moving-to-trash IS-MAC)


;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))


;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setopt save-interprogram-paste-before-kill t)

;; Save clipboard contents into kill-ring before replacing them
(setopt save-interprogram-paste-before-kill t)



;; -----------------------------------------------------------------
;; Garbage collection
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
  If you experience freezing, decrease this.
  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))

            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -----------------------------------------------------------------


;; Modes and mode groupings
(defmacro hook-into-modes (func modes)
  "Add hook `FUNC' to multiple `MODES'."
  `(dolist (mode-hook, modes)
     (add-hook mode-hook, func)))

(provide 'initialsetup)
