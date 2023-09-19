;;; mylisp/mydisplay.el -*- lexical-binding: t; -*-

;; (use-package flyspell
;;   :straight t

;;   :hook ((LaTeX-mode . flyspell-mode)
;;          (latex-mode . flyspell-mode)
;;          (markdown-mode . flyspell-mode)
;;          (org-mode . flyspell-mode)
;;          )
  
;;   :config
;;   (define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word)
;;   (setq ispell-program-name "aspell"
;;         aspell-dictionary "en_GB-ise-wo_accents"
;;         aspell-program-name "/usr/bin/aspell"
;;         ispell-dictionary "en_GB-ise-wo_accents"
;;         ispell-program-name "/usr/bin/aspell")
;;   (unbind-key "C-." flyspell-mode-map)
;;   (unbind-key "C-," flyspell-mode-map)
;;   (when IS-MAC
;;     (progn
;;       (setq ispell-program-name "aspell"
;;             aspell-dictionary "en_GB-ise-wo_accents"
;;             aspell-program-name "/usr/local/bin/aspell"
;;             ispell-dictionary "en_GB-ise-wo_accents"
;;             ispell-program-name "/usr/local/bin/aspell")
;;       (unbind-key "C-." flyspell-mode-map)
;;       (unbind-key "C-," flyspell-mode-map)
;;       (bind-key "C-." 'comment-or-uncomment-line-or-region)
;;       (bind-key "C-," 'comment-or-uncomment-line-or-region)
;;       )
;;     )
;;   (defun make-flyspell-overlay-return-mouse-stuff (overlay)
;;     (overlay-put overlay 'help-echo nil)
;;     (overlay-put overlay 'keymap nil)
;;     (overlay-put overlay 'mouse-face nil))
;;   (advice-add 'make-flyspell-overlay :filter-return #'make-flyspell-overlay-return-mouse-stuff)
;;   )



;; Required in Fedora: enchant2-devel, pkgconf
(use-package jinx
  :straight t
  ;; :hook ((LaTeX-mode . jinx-mode)
  ;;        (latex-mode . jinx-mode)
  ;;        (markdown-mode . jinx-mode)
  ;;        (org-mode . jinx-mode)
  ;;        )
  ;; :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
)


(provide 'myspell)
