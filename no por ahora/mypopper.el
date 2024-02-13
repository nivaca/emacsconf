;; ====================================================================
(use-package popper
  :disabled t
  :straight t
  :init
  (when window-system
    (pcase (system-name)
      ;; PC escritorio casa
      ("nivaca-pc" (bind-keys*
                    ("C-|"   . popper-toggle-latest)
                    ("M-|"   . popper-cycle)
                    ("C-M-|" . popper-toggle-type)))
      ;; XPS 13
      ("nivaca-xps" (bind-keys*
                     ("C-`"   . popper-toggle-latest)
                     ("M-`"   . popper-cycle)
                     ("C-M-`" . popper-toggle-type)))
      ;; TP
      ("nivaca-tp" (bind-keys*
                    ("C-|"   . popper-toggle-latest)
                    ("M-|"   . popper-cycle)
                    ("C-M-|" . popper-toggle-type))))
    ;; Mac
    (when IS-MAC (bind-keys*
                  ("C-|"   . popper-toggle-latest)
                  ("M-|"   . popper-cycle)
                  ("C-M-|" . popper-toggle-type))))
  ;;
  ;; (setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
        '(Custom-mode
          (compilation-mode . hide)
          messages-buffer-mode
          "^\\*Warnings\\*$"
          "^\\*straight-process\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*Matlab Help\\*"
          "^\\*Messages\\*$"
          "^\\*Backtrace\\*"
          "^\\*evil-registers\\*"
          "^\\*Apropos"
          "^Calc:"
          "^\\*TeX errors\\*"
          "^\\*ielm\\*"
          "^\\*TeX Help\\*"
          "\\*Shell Command Output\\*"
          "\\*Completions\\*"
          "\\*scratch\\*"
          "[Oo]utput\\*"))
  (popper-mode +1)
  )
