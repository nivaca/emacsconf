;;; mylisp/myterm.el -*- lexical-binding: t; -*-

(use-package eat
  :straight (:host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi" "*.ti"
                           ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (setq process-adaptive-read-buffering nil)
  )


;; =============================================
(use-package shell-command-plus
  :straight t
  :bind (("M-!" . shell-command+)))

;; =============================================
;; (defun nv-terminal-here ()
;;   "Open terminal in current working directory."
;;   (interactive)
;;   (call-process "/usr/bin/konsole" 
;;                 nil 0 nil "--workdir" default-directory))

(defun nv-terminal-here ()
  "Open Kitty in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (setenv "DISPLAY" ":0")
    (setenv "WAYLAND_DISPLAY" "wayland-0")
    (start-process
     "kitty" nil
     "/home/nivaca/.local/bin/kitty")))

(provide 'myterm)
