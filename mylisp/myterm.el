;;; mylisp/myterm.el -*- lexical-binding: t; -*-

(use-package eat
  :straight
  (eat
   :type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))
   )
  :config
  (setq process-adaptive-read-buffering nil)
  )



;; =============================================
(defun nv-terminal-here ()
  "Open terminal in current working directory."
  (interactive)
  (cond ((eq system-type 'gnu/linux)
         ;; Linux
         (call-process "/usr/bin/konsole" 
          nil 0 nil "--workdir" default-directory)
         ;; (let ((process-environment 
         ;;        (append
         ;;         '("DISPLAY=:0"
         ;;           "WAYLAND_DISPLAY=wayland-0")
         ;;         process-environment)))
         ;;   (start-process "wezterm" nil "/usr/bin/wezterm" "start" "--cwd" default-directory))
         )
        ((eq system-type 'darwin)
         ;; Mac
         (call-process "/Users/nicolasvaughan/bin/iterm" 
          nil 0 nil "--workdir" default-directory)
         )
        )
  )

(provide 'myterm)
