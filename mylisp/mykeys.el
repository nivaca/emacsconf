;;; mylisp/mykeys.el -*- lexical-binding: t; -*-

(global-set-key (quote [f1]) 'nv-switch-to-minibuffer)
(global-set-key (quote [C-f1]) 'goto-last-change)
;; (global-set-key (quote [S-f1]) 'clone-indirect-buffer)

;; (global-unset-key (quote [f2]))
(global-set-key (quote [f2]) 'save-buffer)
(global-set-key (kbd "C-x s") 'save-buffer)

;; (global-set-key (quote [f3]) 'kmacro-start-macro-or-insert-counter)
;; (global-set-key (quote [S-f3]) 'kmacro-end-and-call-macro)

(global-set-key (quote [f4]) 'consult-line)
(global-set-key (quote [S-f4]) 'occur)

;; (global-set-key (quote [f5]) 'org-mode)
;; (global-set-key (quote [f6]) 'lisp-mode)
;; (global-set-key (quote [f7]) 'latex-mode)

;; (global-unset-key (quote [f5]))
;; (global-set-key [f5] 'neotree-toggle)

(global-unset-key (quote [f6]))
;; (global-unset-key (quote [f7]))

(global-set-key (quote [f8]) 'dired)
(global-unset-key (quote [f9]))
(global-unset-key (quote [f10]))

(global-set-key (kbd "C-c t") #'nv-terminal-here)
(global-set-key (kbd "C-c e") #'eat)

(global-set-key (quote [f12]) 'execute-extended-command)
(global-set-key (quote [S-f12]) 'eval-expression)
(global-set-key (quote [C-f12]) 'repeat-complex-command)

(global-set-key (kbd "\C-x\C-m") #'execute-extended-command)

(global-set-key (kbd "M-w") 'nv-select-word)

;; ----------------------------------------------
;; Deactivate dangerous keys
;; (global-unset-key [(control w)])
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-w")
(global-set-key (kbd "\C-w") 'kill-ring-save)  ;; copy

;; kill buffer
(global-set-key (kbd "C-x k")
                #'(lambda () (interactive)
                    (let (kill-buffer-query-functions) (kill-buffer))))

(global-unset-key "\C-v")

(global-set-key (kbd "M-v") #'scroll-down-command)
(global-set-key (kbd "C-c v") #'scroll-up-command)

(global-unset-key (kbd "M-DEL"))

(global-unset-key (kbd "<C-down-mouse-1>"))  ;; disable "buffer menu"
(global-unset-key (kbd "<C-down-mouse-2>"))  ;; disable "buffer menu"
(global-unset-key (kbd "<C-down-mouse-3>"))  ;; disable "buffer menu"
;; (global-unset-key (kbd "<mouse-3>")) ;; disable right click?
(global-unset-key (kbd "<S-down-mouse-1>"))

;; extend region with mouse
;; https://superuser.com/questions/521223/shift-click-to-extend-marked-region
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

(global-set-key (kbd "C-/") #'comment-line) ;; defined in myfunctions.el
(global-set-key (kbd "C-_") #'comment-line)

;; Undo & redo
(global-set-key (kbd "C-z") #'undo-only)
(global-set-key (kbd "C-c z") #'undo-redo)

(global-unset-key (kbd "<S-return>"))


(global-set-key (kbd "C-k") #'kill-region)  ; Cut

;; Unset stardard Emacs copy, cut, paste keys
;; (global-unset-key "\C-k") ; cut
;; (global-unset-key "\C-y") ; yank
;; (global-unset-key "\C-w") ; kill-ring-save


;; https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
         (message
          (substitute-command-keys
           "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
         (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(define-key query-replace-map [escape] 'quit)

;; disable SPC autocomplete in minibuffer
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Abolish secondary selection
(global-set-key [remap mouse-drag-secondary] 'mouse-drag-region)
(global-set-key [remap mouse-set-secondary] 'mouse-set-region)
(global-set-key [remap mouse-start-secondary] 'mouse-set-point)
(global-set-key [remap mouse-yank-secondary] 'mouse-yank-primary)
(global-set-key [remap mouse-secondary-save-then-kill] 'mouse-save-then-kill)

;; defined in myedit.el
(global-set-key (kbd "C-M-n") #'narrow-or-widen-dwim)

;; unset ibuffer
(global-set-key (kbd "C-x C-b") nil)

;; hippie expand: M-/
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)


(global-set-key (kbd "<f5>") 'revert-buffer-quick)

(define-key (current-global-map) (kbd "M-q") 'fill-paragraph)

;; Map escape to cancel (like C-g)...
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(when (display-graphic-p)
  ;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  ;; "Super"" bindings
  (global-set-key (kbd "s-g") #'goto-last-change)
  (global-set-key (kbd "<s-f12>") #'nv-load-config)
  (global-set-key (kbd "<s-escape>") #'nv-kill-all-buffers)

  ;; terminal
  (global-set-key (quote [f10]) 'nv-terminal-here)
  (global-set-key (quote [S-f10]) 'nv-terminal-here)
  (global-set-key (quote [M-f10]) 'eat)

  ;; mouse bindings
  (global-set-key [mode-line mouse-4] #'previous-buffer)
  (global-set-key [mode-line mouse-5] #'next-buffer)

  ;; multiple cursors
  (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)

  ;; mouse bindings
  (global-set-key (kbd "<s-mouse-5>") #'previous-buffer)
  (global-set-key (kbd "<s-mouse-4>") #'next-buffer))


(provide 'mykeys)
