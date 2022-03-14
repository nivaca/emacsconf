;;; mylisp/mycompletions.el -*- lexical-binding: t; -*-

;; ======================================================================
;;                                 Company
;; ======================================================================
;; (use-package company
;;   :straight
;;   :blackout
;;   :config
;;   (global-company-mode)
;;   :custom
;;   (company-idle-delay 0.2)
;;   (company-tooltip-limit 0)
;;   (company-begin-commands '(self-insert-command))
;;   (company-transformers '(company-sort-by-occurrence))
;;   (company-selection-wrap-around t)
;;   (company-minimum-prefix-length 3)
;;   (company-selection-wrap-around t)
;;   ;; (company-dabbrev-downcase nil)
;;   )

;; ;; (add-hook 'after-init-hook 'global-company-mode)

;; (use-package company-auctex
;;   :straight t
;;   :defer t
;;   :config
;;   (company-auctex-init)
;;   (add-to-list 'company-backends '(company-auctex))
;; )

;; (with-eval-after-load 'company
;;   (add-hook 'latex-mode-hook 'company-mode)
;;   (add-hook 'lisp-mode-hook 'company-mode)
;;   )



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
  (corfu-global-mode))



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
