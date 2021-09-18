;;; mylisp/myivy.el -*- lexical-binding: t; -*-

(use-package ivy
  :straight t
  :blackout ivy-mode
  :bind
  ("C-x b" . ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-wrap nil
        ivy-count-format "(%d/%d) "
        ;; do not exit ivi on backspace:
        ivy-on-del-error-function (lambda () nil)
        ivy--regex-ignore-order nil
        ivy-initial-inputs-alist ()
        ivy-re-builders-alistq
        '((t . ivy--regex-ignore-order))
        )
  (define-key ivy-minibuffer-map (kbd "C-w")
    'nv-ivy-yank-whole-word)
  )


(use-package counsel
  :straight t
  :bind
  ("M-x" . counsel-M-x)
  ;; ("C-x b" . counsel-switch-buffer)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  )


(use-package swiper
  :straight t
  :bind  (
          ("s-s" . swiper)
          ;; ("C-s" . )
          ;; ("C-s" . counsel-grep-or-swiper)
          ("<f4>" . swiper-isearch)
          ("C-c C-r" . ivy-resume)
          ("<f6>" . ivy-resume)
          :map ivy-minibuffer-map
          ("M-y" . ivy-next-line)
          ("C-<down>" . ivy-next-history-element)
          ("C-<up>" . ivy-previous-history-element)
          )
  :init
  (setq counsel-grep-base-command
        "rg --sort path -M 120 --no-heading --line-number --color never %s")
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (defun nv-swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter)
    )
  (advice-add 'swiper :after #'nv-swiper-recenter)
  )


(use-package marginalia
  :after ivy
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))



(use-package flx  ;; Improves sorting for fuzzy-matched results
  :straight t
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000)
  )


(provide 'myivy)
