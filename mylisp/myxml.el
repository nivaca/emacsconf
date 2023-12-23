;;; mylisp/myxml.el -*- lexical-binding: t; -*-


(use-package nxml-mode
  :straight 
  :mode ("\\.xml$" . nxml-mode)
  :custom
  (indent-tabs-mode nil)
  (nxml-child-indent 2)
  (nxml-attribute-indent 2)
  ;; Optional settings
  (nxml-slash-auto-complete-flag t)
  )



(defun nv-nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                  (condition-case nil
                      (progn
                        (nxml-backward-up-element) ; always returns nil
                        t)
                    (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))




(provide 'myxml)
