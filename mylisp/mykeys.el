;;; mylisp/mykeys.el -*- lexical-binding: t; -*-

(use-package emacs
  :config
  (global-set-key (quote [f1]) 'nv-switch-to-minibuffer)
  (global-set-key (quote [C-f1]) 'goto-last-change)
  ;; (global-set-key (quote [S-f1]) 'clone-indirect-buffer)

  (global-unset-key (quote [f2]))
  ;; (global-set-key (quote [f2]) 'save-buffer)
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
  (global-set-key (quote [f10]) 'nv-terminal-here)
  (global-set-key (quote [S-f10]) 'nv-terminal-here)
  (global-set-key (quote [M-f10]) 'eat)

  (global-set-key (quote [f12]) 'execute-extended-command)
  (global-set-key (quote [S-f12]) 'eval-expression)
  (global-set-key (quote [C-f12]) 'repeat-complex-command)

  (if IS-LINUX
      (global-set-key (quote [s-f12]) 'nv-load-config))

  (bind-key "\C-x\C-m" 'execute-extended-command)

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

  (global-unset-key (kbd "M-DEL"))

  ;; Map escape to cancel (like C-g)...
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))


  (global-unset-key (kbd "<C-down-mouse-1>"))  ;; disable "buffer menu"
  (global-unset-key (kbd "<C-down-mouse-2>"))  ;; disable "buffer menu"
  (global-unset-key (kbd "<C-down-mouse-3>"))  ;; disable "buffer menu"
  ;; (global-unset-key (kbd "<mouse-3>")) ;; disable right click?
  (global-unset-key (kbd "<S-down-mouse-1>"))

  ;; extend region with mouse
  ;; https://superuser.com/questions/521223/shift-click-to-extend-marked-region
  (define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

  (bind-key "C-/" 'comment-line) ;; defined in myfunctions.el


  ;; Undo & redo
  (bind-key "C-z" 'undo-only)
  (bind-key "C-S-z" 'undo-redo)
  
  (global-unset-key (kbd "<S-return>"))


  (bind-key "C-k" 'kill-region)  ; Cut
  
  ;; Unset stardard Emacs copy, cut, paste keys
  ;; (global-unset-key "\C-k") ; cut
  ;; (global-unset-key "\C-y") ; yank
  ;; (global-unset-key "\C-w") ; kill-ring-save

  ;; ====== previous and next buffer with mouse wheel =======
  (global-set-key (kbd "<s-mouse-5>") 'previous-buffer);
  (global-set-key (kbd "<s-mouse-4>") 'next-buffer);

  )



(use-package emacs
  :config
  ;; (advice-add 'keyboard-quit :around #'my-keyboard-quit-advice)
  ;; https://with-emacs.com/posts/tips/quit-current-context/
  (defun keyboard-quit-context+ ()
    "Quit current context. This function is a combination of
`keyboard-quit' and `keyboard-escape-quit' with some parts omitted and
some custom behavior added."
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
  )



(use-package emacs
  :config
  (define-key query-replace-map [escape] 'quit)
  
  ;; disable SPC autocomplete in minibuffer
  (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

  ;; Abolish secondary selection
  (global-set-key [remap mouse-drag-secondary] 'mouse-drag-region)
  (global-set-key [remap mouse-set-secondary] 'mouse-set-region)
  (global-set-key [remap mouse-start-secondary] 'mouse-set-point)
  (global-set-key [remap mouse-yank-secondary] 'mouse-yank-primary)
  (global-set-key [remap mouse-secondary-save-then-kill] 'mouse-save-then-kill)

  ;; Switch buffers with mouse scroll on mode-line
  (global-set-key [mode-line mouse-4] 'previous-buffer)
  (global-set-key [mode-line mouse-5] 'next-buffer)

  ;; Kill all buffers
  (bind-key "<s-escape>" 'nv-kill-all-buffers)
  ;; (bind-key "<s-escape>" nil)
  ;; (unbind-key "<s-escape>")

  ;; defined in myedit.el
  (bind-key "C-M-n" 'narrow-or-widen-dwim)

  ;; unset ibuffer
  (global-set-key (kbd "C-x C-b") nil)

  ;; multiple-cursors
  (when IS-LINUX
    (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

  ;; hippie expand: M-/
  (global-set-key [remap dabbrev-expand] 'hippie-expand)

  ;; helpful
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  )


;; ================================================
;; Mac settings
(use-package emacs
  :if IS-MAC
  :config
  (setq mac-right-option-modifier 'none
        mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil))
        mouse-wheel-progressive-speed nil
        mac-command-modifier 'control
        mac-control-modifier 'meta
        mac-pass-command-to-system nil
        )
  ;; Kill all buffers
  (bind-key "<M-escape>" 'nv-kill-all-buffers)
  (bind-key "C--" 'comment-line) ;; defined in myfunctions.el
  (bind-key "S-<down-mouse-1>" 'mouse-set-mark)
  (bind-key "S-<mouse-1>" 'mouse-set-mark)
  ;;
  ;; disable context menu
  (context-menu-mode -1)
  ;; multiple-cursors
  (global-set-key (kbd "<C-mouse-1>") 'mc/add-cursor-on-click)
  ;; help
  (global-set-key (kbd "C-h") 'help-command)
  )




(global-set-key (kbd "<f5>") 'revert-buffer-quick)




(provide 'mykeys)
