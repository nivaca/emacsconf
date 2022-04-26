;;; mylisp/mycompletions.el -*- lexical-binding: t; -*-

;; ============================================================
;;                              Corfu
;; ============================================================
;; Corfu enhances completion at point with a small completion popup.
;; The current candidates are shown in a popup below or above the point.
(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  (corfu-auto t)
  (corfu-auto-prefix 0)
  (corfu-auto-delay 0.5)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  (completion-cycle-threshold 3)
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))



;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))




(provide 'mycompletions)
;;; mycompletions.el ends here
