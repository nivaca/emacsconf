;;; mylisp/myxml.el -*- lexical-binding: t; -*-


(use-package nxml-mode
  :straight
  :defer t
  :mode ("\\.xml$" . nxml-mode)
  :custom
  (indent-tabs-mode nil)
  (tab-width 2)
  (indent-line-function 'insert-tab)
  (nxml-child-indent 2)
  (nxml-attribute-indent 2)
  (nxml-in )
  (rng-validate-chunk-size 4000)
  (rng-validate-quick-delay 1)
  (rng-validate-delay 2)
  (nxml-slash-auto-complete-flag t)
  ;;
  :config
  (defun nv-nxml-surround-region-with-tag (tag-name beg end)
    "Place TAG-NAME around region from BEG to END."
    (interactive "sTag name: \nr")
    (save-excursion
      (goto-char beg)
      (insert "<" tag-name ">")
      (goto-char (+ end 2 (length tag-name)))
      (insert "</" tag-name ">"))
    )
  ;;
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
  ;;
  :bind (:map nxml-mode-map
              ("C-c e" . nv-nxml-surround-region-with-tag)
              ("C-c w" . nv-nxml-where)
              )
  ) ;; end use-package

(require 'myxmlsnippets)

(provide 'myxml)
