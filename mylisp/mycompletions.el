;;; mylisp/mycompletions.el -*- lexical-binding: t; -*-


;;; Code:

;; Larger read chunks help LSP/eglot-backed completion feel snappier.
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

;; Keep `text-mode' from adding `ispell-completion-at-point' to
;; `completion-at-point-functions'.  That capf greps a flat English
;; word list and ignores the buffer's language entirely.
(setq text-mode-ispell-word-completion nil)


;; ============================================================
;;                              Corfu
;; ============================================================
;; Corfu enhances completion at point with a
;; small completion popup. The current candidates
;; are shown in a popup below or above the point.
(use-package corfu
  ;; :disabled
  :straight t
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  :custom
  (completion-cycle-threshold 3)
  (corfu-auto t)
  (corfu-auto-delay 0.4)          ; was 0.2 — how long before the popup appears
  (corfu-auto-prefix 3)           ; was 2 — how many chars before it may appear
  (corfu-cycle t)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(2.0 . 1.0)) ; (initial . subsequent) doc-panel delay
  (corfu-preselect 'first)        ; replaces the obsolete `corfu-preselect-first'
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  ;; `corfu-echo-documentation' is obsolete; echo-area docs are now
  ;; `corfu-echo-mode' + `corfu-echo-delay'.  Left off on purpose:
  ;; `corfu-popupinfo-mode' below already shows documentation.
  ;;
  ;; (corfu-preview-current nil)
  ;; (corfu-quit-at-boundary t)
  :bind
  (:map corfu-map
        ("RET" . corfu-insert)
        ([ret] . corfu-insert)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-SPC" . corfu-insert-separator)
        ;; In a GUI, <tab> and TAB are distinct events: the former completes,
        ;; the latter falls through to the global binding (indentation).
        ("<tab>" . corfu-complete)
        ("TAB" . nil))
  :config
  ;; Documentation popup next to the candidate list.
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (require 'corfu-history)
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))



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
  (completion-preview-minimum-symbol-length 3) ; was 2 — show after three chars
  (completion-preview-exact-match-only nil) ; If t, only show suggestion if there is only one candidate
  (completion-preview-idle-delay 1.5) ; was 1 — idle seconds before the preview
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


;; ==================== dabbrev ====================
;; Governs both `M-/' and `cape-dabbrev' below.
(use-package dabbrev
  :straight nil
  :custom
  (dabbrev-case-replace nil)        ; keep the candidate's own capitalisation
  (dabbrev-check-other-buffers nil) ; only the current buffer
  (dabbrev-check-all-buffers nil))


;; ==================== cape ====================
(use-package cape
  :straight t
  :custom
  (cape-dabbrev-min-length 4)
  (cape-dabbrev-check-other-buffers nil)
  :config
  ;; Complete from words already present in the buffer — language-agnostic,
  ;; so Spanish prose yields Spanish candidates.
  (add-hook 'text-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)))

  ;; Org's pcomplete capf is exclusive: it comes first in
  ;; `completion-at-point-functions' and, when it finds no match, stops
  ;; completion instead of falling through to `cape-dabbrev'.  Wrapping it
  ;; keeps its Org-specific completions (#+keywords, tags, TODO states,
  ;; link types) while letting the rest of the list run.
  (defun my/org-capf-nonexclusive ()
    "Stop Org's pcomplete capf from shadowing later capfs."
    (setq-local completion-at-point-functions
                (mapcar (lambda (f)
                          (if (eq f #'pcomplete-completions-at-point)
                              (cape-capf-nonexclusive f)
                            f))
                        completion-at-point-functions)))

  (add-hook 'org-mode-hook #'my/org-capf-nonexclusive 90))


(provide 'mycompletions)
;;; mycompletions.el ends here

;; Local Variables:
;; jinx-languages: "en_US"
;; End:
