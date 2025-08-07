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
         ;; (call-process "/usr/bin/konsole" 
         ;; nil 0 nil "--workdir" default-directory)
         ;; (let ((process-environment 
         ;;        (append process-environment 
         ;;                (list (format "WAYLAND_DISPLAY=%s" (or (getenv "WAYLAND_DISPLAY") "wayland-0"))
         ;;                      (format "XDG_SESSION_TYPE=%s" (or (getenv "XDG_SESSION_TYPE") "wayland"))))))
         ;;   (start-process "kitty" nil "kitty" "--directory" default-directory))
         (let ((process-environment 
                (append process-environment 
                        (list (format "WAYLAND_DISPLAY=%s" (or (getenv "WAYLAND_DISPLAY") "wayland-0"))
                              (format "XDG_SESSION_TYPE=wayland")
                              (format "XDG_RUNTIME_DIR=%s" (or (getenv "XDG_RUNTIME_DIR") "/run/user/1000"))))))
           (start-process "kitty" nil "kitty" "--directory" default-directory))
         )
        ((eq system-type 'darwin)
         ;; Mac
         ;; (call-process "/Users/nicolasvaughan/bin/iterm" 
         ;; nil 0 nil "--workdir" default-directory)
         (call-process "/Applications/kitty.app/Contents/MacOS/kitty" 
                       nil 0 nil "--directory" default-directory))))

(provide 'myterm)
