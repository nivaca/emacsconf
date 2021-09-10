;;; mylisp/mynxml.el -*- lexical-binding: t; -*-

(defun my-nxml-hook ()
  (progn
    (bind-key "M-SPC" 'compilation-at-point nxml-mode-map)
    (lsp)
    )
  )

(add-hook 'nxml-mode-hook 'my-nxml-hook)


(provide 'mynxml)
;;; mynxml.el ends here
