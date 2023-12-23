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
  (corfu-preselect-first 1)   ;; Candidate preselection
  (corfu-auto t)              ;; Enable auto completion
  (corfu-auto-prefix 2)       ;; Enable auto completion
  (corfu-auto-delay 1)      ;; Enable auto completion
  (corfu-commit-predicate nil)
  ;; (corfu-preview-current nil)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  (completion-cycle-threshold 3)
  
  
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




;; --------------- Temple templating engine -------------
(use-package tempel
  ;; templates are stored in ~/emacs/templates
  ;;
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode)
  )



(provide 'mycompletions)
;;; mycompletions.el ends here
