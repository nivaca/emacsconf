;;; mylisp/myxml.el -*- lexical-binding: t; -*-

(message "Loading %s..." load-file-name)


;;;; Helper functions

(defun nv-nxml-indent-settings ()
  "Set indentation preferences for XML buffers."
  (setq indent-tabs-mode nil
        tab-width 2))


(defun nv-wrap-element-with-tag (tag-name beg end)
  "Wrap region from BEG to END with TAG-NAME."
  (interactive "sTag name: \nr")
  (save-excursion
    (goto-char beg)
    (insert "<" tag-name ">")
    (goto-char (+ end 2 (length tag-name)))
    (insert "</" tag-name ">")))


(defun nv-nxml-where ()
  "Display XML hierarchy path at point.
Returns the path when called from Lisp."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point))
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element)
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (let ((result (concat "/" (mapconcat #'identity path "/"))))
          (if (called-interactively-p 'interactive)
              (message "%s" result)
            result))))))


;;;; Configure built-in nxml-mode safely

(with-eval-after-load 'nxml-mode

  ;; indentation
  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)

  ;; validation responsiveness
  (setq rng-validate-chunk-size 4000)
  (setq rng-validate-quick-delay 1)
  (setq rng-validate-delay 2)

  ;; auto-close tags
  (setq nxml-slash-auto-complete-flag t)

  ;; local indentation preferences
  (add-hook 'nxml-mode-hook #'nv-nxml-indent-settings)

  ;; enable RELAX NG validation when schemas exist
  (add-hook 'nxml-mode-hook #'rng-validate-mode)

  ;; helper keybindings
  (define-key nxml-mode-map (kbd "C-c e") #'nv-wrap-element-with-tag)
  (define-key nxml-mode-map (kbd "C-c w") #'nv-nxml-where))


;;;; Optional snippet bindings (safe load order)

(with-eval-after-load 'nxml-mode
  (with-eval-after-load 'myxmlsnippets
    (define-key nxml-mode-map (kbd "s-e") #'nv-xml-snippets)
    (define-key nxml-mode-map (kbd "s-w") #'nv-xml-snippets)))


(provide 'myxml)