;;; mylisp/mycompletions.el -*- lexical-binding: t; -*-

;; ============================================================
;;                              Corfu
;; ============================================================
;; Corfu enhances completion at point with a small completion popup.
;; The current candidates are shown in a popup below or above the point.
(use-package corfu
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
  (global-corfu-mode)
  )


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
