;;; mylisp/myaliases.el -*- lexical-binding: t; -*-

;; Some aliases
(defalias 'er 'eval-region)
(defalias 'eb 'eval-buffer)
(defalias 'rs 'replace-string)
(defalias 'rr 'replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'p-u-p 'straight-x-pull-all)
;; (defalias 'p-u-p 'paradox-upgrade-packages)
;; (defalias 'p-l-p 'package-list-packages)
(defalias 'r-o-m 'read-only-mode)


(provide 'myaliases)
