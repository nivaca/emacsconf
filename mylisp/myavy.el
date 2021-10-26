;;; mylisp/myavy.el -*- lexical-binding: t; -*-

(use-package avy
  :defer t
  :straight
  :bind
  ("M-s" . avy-goto-char)
  ("M-j" . avy-goto-char-timer)
  ("M-g g" . avy-goto-line)
  )


(provide 'myavy)
