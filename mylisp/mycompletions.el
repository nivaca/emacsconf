;;; mylisp/mycompletions.el -*- lexical-binding: t; -*-

;; ============================================================
;;                              Corfu
;; ============================================================
;; Corfu enhances completion at point with a small completion popup.
;; The current candidates are shown in a popup below or above the point.
(use-package corfu
  :straight t
  ;; TAB-and-Go customizations
  :custom
  (completion-cycle-threshold 3)
  (completion-cycle-threshold 3)
  (corfu-auto t)              ;; Enable auto completion
  (corfu-auto-delay 1)      ;; Enable auto completion
  (corfu-auto-prefix 2)       ;; Enable auto completion
  (corfu-commit-predicate nil)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-echo-documentation nil)
  (corfu-preselect-first 1)   ;; Candidate preselection
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (tab-always-indent 'complete)
  ;; (corfu-preview-current nil)
  ;; (corfu-quit-at-boundary t)
  ;;
  :bind
  (:map corfu-map
        ("RET" . corfu-insert)
        ([ret] . corfu-insert)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-SPC" . corfu-insert-separator)
        ("TAB"     . nil)  ;; leave my enter alone!
        )
  :init
  (global-corfu-mode))


;; ===============  Yasnippet  ===============
(use-package yasnippet
  :straight t
  :config
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (setq
   yas-use-menu nil
   yas-verbosity 3
   yas-indent-line nil
   yas-wrap-around-region t
   yas-snippet-dirs (append yas-snippet-dirs '("~/emacs/snippets")))
  (yas-global-mode t)
  (blackout 'yas-minor-mode)
  (yas-reload-all))





(provide 'mycompletions)
;;; mycompletions.el ends here
