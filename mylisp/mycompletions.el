;;; mylisp/mycompletions.el -*- lexical-binding: t; -*-


;;; Code:

;; ============================================================
;;                              Corfu
;; ============================================================
;; Corfu enhances completion at point with a small completion popup.
;; The current candidates are shown in a popup below or above the point.
(use-package corfu
  ;; :disabled
  :straight t
  :init
  (setq tab-always-indent 'complete)
  :config
  (define-key corfu-map (kbd "<tab>") #'corfu-complete)
  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :custom
  (completion-cycle-threshold 3)
  (completion-cycle-threshold 3)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-commit-predicate nil)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)
  (corfu-preselect-first 1)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (tab-always-indent 'complete)
  (read-process-output-max (* 4 1024 1024)) ; 4MB
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


;; ==================== completion-preview ====================
(use-package completion-preview
  :straight nil
  :hook (after-init . global-completion-preview-mode)
  :bind
  ( :map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 2) ; Show the preview already after two symbol characters
  (completion-preview-exact-match-only nil) ; If t, only show suggestion if there is only one candidate
  (completion-preview-idle-delay 1) ; If non-nil, wait this many idle seconds before displaying preview
  :config
  (with-eval-after-load 'org
    ;; Add Org mode's custom 'self-insert-command' to completion-previews
    (push 'org-self-insert-command completion-preview-commands)
    )
  ;; Disable completion preview in Org tables (Emacs 31+)
  (defun my/detect-org-table ()
    "Return true if point in Org table."
    (and (derived-mode-p 'org-mode) (org-at-table-p)))
  (add-hook 'completion-preview-inhibit-functions
            #'my/detect-org-table))


(provide 'mycompletions)
;;; mycompletions.el ends here

;; Local Variables:
;; jinx-languages: "en_US"
;; End:
