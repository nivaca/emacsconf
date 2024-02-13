;;; ============== mypython.el ==============

;; (use-package elpy
;;   :defer 2
;;   :ensure t
;;   :config
;;   (setq elpy-modules '(elpy-module-sane-defaults
;;                        elpy-module-company
;;                        elpy-module-eldoc
;;                        ;;elpy-module-pyvenv)
;;                        ))
;;   (setq py-python-command "/usr/local/bin/python3")
;;   (diminish 'elpy-mode "☕")
;;   (elpy-enable)
;;   )

; (use-package anaconda-mode
;   :ensure t
;   :commands anaconda-mode
;   :init
;   (add-hook 'python-mode-hook 'anaconda-mode)
;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;   (diminish 'anaconda-mode "☕")
;   )

; (use-package company-anaconda
;   :ensure t
;   :init (add-to-list 'company-backends 'company-anaconda))

(provide 'mypython)
