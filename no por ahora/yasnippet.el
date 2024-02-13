;; ===============  Yasnippet  ===============
(use-package yasnippet
  :functions yas-global-mode
  :blackout yas-minor-mode
  :defer 3
  :config
  (setq
   yas-verbosity 3
   yas-indent-line nil
   ;; This replaces the default dirs:
   ;; yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
   ;; This appends:
   yas-snippet-dirs (append yas-snippet-dirs
                            '("~/emacs/snippets"))
   )
  (yas-global-mode t)
  (yas-reload-all)
  )
