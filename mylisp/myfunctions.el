;;; mylisp/myfunctions.el -*- lexical-binding: t; -*-


(defun nv-display-configuration-reload ()
  (interactive)
  (load-file (concat user-emacs-directory "mylisp/mydisplay.el")))


(defun nv-emacs-configuration-reload ()
  (interactive)
  (load-file (concat user-emacs-directory "myinit.el")))

  
(defun nv-latex-remove-command ()
  "Unwrap the command that point is in.  By command we understand
a symbol starting with \\ and followed by a block of text
enclosed in {}."
  (interactive)
  (let ((ok (sp-get-enclosing-sexp)))
    (when ok
      (save-excursion                  ;; this deletes the command name and \
        (goto-char (sp-get ok :beg))
        (zap-to-char -1 ?\\ ))
      (sp-splice-sexp))))              ;; remove the enclosing {}


(defun nv-eliminatemacron ()
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "ā" nil t)
      (replace-match "a"))
    (goto-char (point-min))
    (while (search-forward "ē" nil t)
      (replace-match "e"))
    (goto-char (point-min))
    (while (search-forward "ī" nil t)
      (replace-match "i"))
    (goto-char (point-min))
    (while (search-forward "ō" nil t)
      (replace-match "o"))
    (goto-char (point-min))
    (while (search-forward "ū" nil t)
      (replace-match "u"))
    (goto-char (point-min))
    (while (search-forward "Ā" nil t)
      (replace-match "A"))
    (goto-char (point-min))
    (while (search-forward "Ē" nil t)
      (replace-match "E"))
    (goto-char (point-min))
    (while (search-forward "Ī" nil t)
      (replace-match "I"))
    (goto-char (point-min))
    (while (search-forward "Ō" nil t)
      (replace-match "O"))
    (goto-char (point-min))
    (while (search-forward "Ū" nil t)
      (replace-match "U"))

  )
)


(defun nv-replace-in-buffer ()
  "Replace text in whole buffer. The suggested OLD text is either the current region, or the next word (as mark-word would select it). The suggested text for the replacement is the same as the OLD text."
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setq curr-word (buffer-substring-no-properties (mark) (point)))
    (setq old-string (read-string "OLD string:\n" curr-word))
    (setq new-string (read-string "NEW string:\n" old-string))
    (query-replace old-string new-string nil (point-min) (point-max))
    )
  )


(defun nv-query-replace (from-string to-string &optional delimited start end)
  "Replace some occurrences of FROM-STRING with TO-STRING.  As each match is found, the user must type a character saying what to do with it. This is a modified version of the standard `query-replace' function in `replace.el', This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer. For more information, see the documentation of `query-replace'"
  (interactive
   (let ((common
      (query-replace-read-args
       (concat "Query replace"
           (if current-prefix-arg " word" "")
           (if (and transient-mark-mode mark-active) " in region" ""))
       nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
       (if (and transient-mark-mode mark-active)
           (region-beginning)
         (buffer-end -1))
       (if (and transient-mark-mode mark-active)
           (region-end)
         (buffer-end 1)))))
  (perform-replace from-string to-string t nil delimited nil nil start end))


(defun nv-query-replace-regexp (regexp to-string &optional delimited start end)
  "Replace some things after point matching REGEXP with TO-STRING.  As each
match is found, the user must type a character saying what to do with
it. This is a modified version of the standard `query-replace-regexp'
function in `replace.el', This modified version defaults to operating on the
entire buffer instead of working only from POINT to the end of the
buffer. For more information, see the documentation of `query-replace-regexp'"
  (interactive
   (let ((common
      (query-replace-read-args
       (concat "Query replace"
           (if current-prefix-arg " word" "")
           " regexp"
           (if (and transient-mark-mode mark-active) " in region" ""))
       t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
       (if (and transient-mark-mode mark-active)
           (region-beginning)
         (buffer-end -1))
       (if (and transient-mark-mode mark-active)
           (region-end)
         (buffer-end 1)))))
  (perform-replace regexp to-string t t delimited nil nil start end))



(defun nv-highlight ()
  "Highlights add and del tags."
  (interactive)
  ;; (highlight-regexp "<add.*?</add>" 'hi-green)
  ;; (highlight-regexp "<del.*?</del>" 'hi-pink)
  ;; (highlight-regexp "<lb.*?/>" 'hi-pink)
  (highlight-regexp "<cb.*?/>" 'hi-pink)
  )

(defun nv-align-repeat (start end regexp)
    "Repeat alignment with respect to
the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 t))


(defun nv-unfill-paragraph ()
 "Takes a multi-line paragraph and makes it into a single line of text."
 (interactive)
 (let ((fill-column (point-max)))
   (fill-paragraph nil)))


(defun nv-remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match " " nil t))))



;; =========== kill all buffers =============
;; https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun nv-kill-all-buffers ()
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (buffer-list))
  )



;; ================ move=text ================
;; http://stackoverflow.com/questions/3156450/shift-a-region-or-line-in-emacs
;;
(defun nv-shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun nv-shift-right (count)
  (interactive "p")
  (nv-shift-text count))

(defun nv-shift-left (count)
  (interactive "p")
  (nv-shift-text (- count)))
;; =============================================



;; =============================================
(defun nv-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


;; =============================================
(defun nv-terminal-here ()
  "Open terminal in current working directory."
  (interactive)
  (cond ((eq system-type 'gnu/linux)
         ;; Linux
         (call-process "konsole" nil 0 nil "--workdir" default-directory)
         )
        ((eq system-type 'darwin)
         ;; Mac
         ;; (call-process "/Users/nicolasvaughan/bin/iterm" nil 0 nil "--workdir" default-directory)
         (call-process (expand-file-name "~/bin/iterm") nil 0 nil "--workdir" default-directory)
         )
        )
  )



;; =============================================

(defun nv-select-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
If you use a negative prefix arg then select words backward.
Otherwise select them forward.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)"
  (interactive "P\np")
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))



;; ;; =============================================
;; (defun nv-ivy-yank-whole-word ()
;;   "Pull nextnext word from buffer into search string."
;;   (interactive)
;;   (let (amend)
;;     (with-ivy-window
;;       ;;move to last word boundary
;;       (re-search-backward "\\b")
;;       (let ((pt (point))
;;             (le (line-end-position)))
;;         (forward-word 1)
;;         (if (> (point) le)
;;             (goto-char pt)
;;           (setq amend (buffer-substring-no-properties pt (point))))))
;;     (when amend
;;       (insert (replace-regexp-in-string "  +" " " amend)))))



(defun nv-byte-recompile-my-files ()
  (interactive)
  (byte-recompile-directory (file-name-as-directory user-lisp-directory) 1 1)
  (byte-recompile-file (concat (file-name-as-directory user-emacs-directory) "myinit.el") 1 1)
  )











(provide 'myfunctions)
