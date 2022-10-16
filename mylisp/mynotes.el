;;; mylisp/mynotes.el -*- lexical-binding: t; -*-

(use-package denote
  :straight t
  :init
  :custom
  (
   (denote-directory (expand-file-name "~/denote/"))
   (denote-known-keywords '("computing" "emacs" "varios"))
   (denote-front-matter-date-format 'org-timestamp)
   (denote-dired-directories
    (list denote-directory
          (expand-file-name "attachments" denote-directory)))
   (denote-date-prompt-use-org-read-date t)
   (denote-date-format nil)
   (denote-link-fontify-backlinks t)
   )
  :config
  (progn
    (defun denote-find-file (filename)
      "Open FILENAME, a denote file.
Interactively ask which file to open with completion."
      (interactive (list (denote--retrieve-read-file-prompt)))
      (find-file filename))))

(provide 'mynotes)
