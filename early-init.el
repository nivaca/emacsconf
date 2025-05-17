;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

(message ">>>>> loading early-init.el")

;; Defer garbage collection further back in the startup process
(setopt
 gc-cons-threshold most-positive-fixnum
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent
 )

;; Require Common Lisp (for "case" etc.)
(eval-when-compile (require 'cl-lib))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the font.
;; By inhibiting this, we easily halve startup times with fonts that are larger than the system default.
(setopt frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)


;; ---------------------------------------------------

;; If non-nil, packages are made available before
;; reading the init file
;; (but after reading the early init file)
(setopt package-enable-at-startup nil)

;; use-package
;; recommended here to reduce load times
(eval-when-compile
  (require 'use-package))
(require 'bind-key) 
