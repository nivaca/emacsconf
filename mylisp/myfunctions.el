;;; mylisp/myfunctions.el -*- lexical-binding: t; -*-

(defun nv-remove-bracketed-content ()
  "Remove contents inside square brackets and parentheses from selected region.
Keeps the brackets/parentheses themselves. Does nothing if no region is active."
  (interactive)
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          ;; Remove contents of square brackets, keeping []
          (while (re-search-forward "\\[[^]]*\\]" nil t)
            (replace-match "[]"))
          ;; Remove contents of parentheses, keeping ()
          (goto-char (point-min))
          (while (re-search-forward "([^)]*)" nil t)
            (replace-match "()")))))))


;; ----------------------------------------------------------------

(defun nv-disable-custom-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  )


;; ====================================================
(defun nv-highlight-non-ascii ()
  (interactive)
  (highlight-regexp "[^[:ascii:]]" 'hi-yellow))

;; ====================================================
(defun nv-wrap-lines-with-l-tags ()
  "Wrap each line in the selected region with <l> and </l>, 
   except lines containing <pb>. Leading whitespace is preserved."
  (interactive)
  (when (use-region-p)  ;; Proceed only if a region is selected
    (save-excursion
      (let ((start (region-beginning))
            (end (copy-marker (region-end)))) ;; Use a marker to avoid shifting the endpoint
        (goto-char start)
        (while (< (point) end)
          (beginning-of-line)
          (let ((line-start (point)))
            (end-of-line)
            (let* ((line-end (point))
                   (line-text (buffer-substring-no-properties line-start line-end))
                   (trimmed-line (string-trim-left line-text))  ;; Remove leading whitespace
                   (leading-space (progn (string-match "^[[:space:]]*" line-text)
                                         (match-string 0 line-text))))  ;; Capture leading spaces
              ;; Skip lines that contain "<pb" but do NOT advance again in the loop
              (if (string-match "<pb[[:space:]]*[^>]*>" trimmed-line)
                  nil  ;; Do nothing, just let the loop advance normally
                (progn
                  (delete-region line-start line-end)  ;; Remove original line content
                  (insert (format "%s<l>%s</l>" leading-space trimmed-line))))))
          (forward-line 1))))))





;; =============================================
;; Source: https://www.emacswiki.org/emacs/misc-cmds.el
(defun nv-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; =============================================

(defun nv-org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))

(defun nv-shutdown-emacs-server () (interactive)
       (when (not (eq window-system 'x))
         (message "Initializing x windows system.")
         (x-initialize-window-system)
         (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
         (select-frame (make-frame-on-display x-display-name '((window-system . x))))
         )
       (let ((last-nonmenu-event nil)(window-system "x"))(save-buffers-kill-emacs))
       )


;; =============================================

(defun nv-reload-emacs-configuration ()
  (interactive)
  ;; (load-file (concat user-emacs-directory "myinit.el"))
  (load-file user-init-file)
  (load-file user-init-file)
  )

;; =============================================

;; define function to shutdown emacs server instance
(defun nv-server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; =============================================

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


;; =============================================

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

;; =============================================

(defun nv-replace-in-buffer ()
  "Replace text in whole buffer.
If a region is active, the default OLD string is the region text.
Otherwise, the OLD string is blank."
  (interactive)
  (save-excursion
    (let* ((curr-word (if (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))
                        ""))
           (old-string (read-string "OLD string:\n" curr-word))
           (new-string (read-string "NEW string:\n" old-string)))
      (query-replace old-string new-string nil (point-min) (point-max)))))


;; (defun nv-replace-in-buffer ()
;;   "Replace text in whole buffer. The suggested OLD text is either the current region, or the next word (as mark-word would select it). The suggested text for the replacement is the same as the OLD text."
;;   (interactive)
;;   (save-excursion
;;     (if (equal mark-active nil) (mark-word))
;;     (setq curr-word (buffer-substring-no-properties (mark) (point)))
;;     (setq old-string (read-string "OLD string:\n" curr-word))
;;     (setq new-string (read-string "NEW string:\n" old-string))
;;     (query-replace old-string new-string nil (point-min) (point-max))
;;     )
;;   )


;; =============================================
(defun nv-replace-in-buffer-case-sensitive ()
  "Query-replace literal text in buffer (always case-sensitive).
- Always case-sensitive
- Literal replacement (safe for LaTeX backslashes)
- Respects narrowing
- Uses active region as default OLD text
- Starts from beginning of accessible buffer"
  (interactive)
  (let* ((case-fold-search nil)   ;; always case-sensitive
         (case-replace nil)
         (default-old
          (if (use-region-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            ""))
         (old (read-string "OLD string: " default-old
                           'nv-replace-history))
         (new (read-string "NEW string: " old
                           'nv-replace-history)))
    (save-excursion
      (goto-char (point-min))     ;; start at beginning (of narrowing)
      (query-replace old new))))


;; =============================================


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


;; =============================================

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


;; =============================================
(defun nv-tex-safe-slug (beg end)
  "Convert region to a TeX-safe slug and abbreviate if longer than 40 chars."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))

  ;; ensure normalization is available
  (require 'ucs-normalize nil t)

  (let* ((maxlen 60)
         (txt (buffer-substring-no-properties beg end)))

    ;; --- normalize ---
    (setq txt (downcase txt))

    ;; remove accents if possible
    (when (fboundp 'ucs-normalize-NFD-string)
      (setq txt (ucs-normalize-NFD-string txt))
      (setq txt (replace-regexp-in-string "\\p{Mn}" "" txt)))

    ;; whitespace → hyphen
    (setq txt (replace-regexp-in-string "\\s-+" "-" txt))
    ;; keep safe chars only
    (setq txt (replace-regexp-in-string "[^a-z0-9:-]" "" txt))
    ;; collapse hyphens
    (setq txt (replace-regexp-in-string "-+" "-" txt))
    ;; trim ends
    (setq txt (replace-regexp-in-string "\\`-\\|-\\'" "" txt))

    ;; --- abbreviate if too long ---
    (when (> (length txt) maxlen)
      (let* ((words (split-string txt "-"))
             (abbr
              (mapconcat
               (lambda (w)
                 (if (> (length w) 6)
                     (substring w 0 4)
                   w))
               words "-")))
        (setq txt
              (if (> (length abbr) maxlen)
                  (substring abbr 0 maxlen)
                abbr))))

    (delete-region beg end)
    (insert txt)))

;; =============================================

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


;; =============================================


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



;; ================ move-text ================
;; https://stackoverflow.com/a/3156642
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
(defun nv-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


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

;; =============================================


(defun nv-byte-recompile-my-files ()
  (interactive)
  (byte-recompile-directory (file-name-as-directory user-lisp-directory) 1 1)
  (byte-recompile-file (concat (file-name-as-directory user-emacs-directory) "myinit.el") 1 1)
  )


;; =============================================

;; org-table-transform-in-place ()
;; https://stackoverflow.com/a/38277039

(defun org-table-transform-in-place ()
  "Just like `ORG-TABLE-EXPORT', but instead of exporting to a
  file, replace table with data formatted according to user's
  choice, where the format choices are the same as
  org-table-export."
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align)
  (let* ((format
          (completing-read "Transform table function: "
                           '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
                             "orgtbl-to-html" "orgtbl-to-generic"
                             "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
                             "orgtbl-to-unicode")))
         (curr-point (point)))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
        (let ((transform (intern (match-string 1 format)))
              (params (and (match-end 2)
                           (read (concat "(" (match-string 2 format) ")"))))
              (table (org-table-to-lisp
                      (buffer-substring-no-properties
                       (org-table-begin) (org-table-end)))))
          (unless (fboundp transform)
            (user-error "No such transformation function %s" transform))
          (save-restriction
            (with-output-to-string
              (delete-region (org-table-begin) (org-table-end))
              (insert (funcall transform table params) "\n")))
          (goto-char curr-point)
          (beginning-of-line)
          (message "Tranformation done."))
      (user-error "Table export format invalid"))))


;; =============================================

;; titlecase.el
;; https://github.com/ap/titlecase

(defvar titlecase-command "titlecase")

(defconst titlecase-buffer "*titlecase output*")

(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (with-temp-buffer
    (insert str)
    (call-process-region (point-min) (point-max) titlecase-command t t nil)
    ;; Skip trailing newline omitted by titlecase
    (buffer-substring (point-min) (1- (point-max)))))

(defun titlecase-region (begin end)
  "Convert text in region from BEGIN to END to title case."
  (interactive "*r")
  (let ((pt (point)))
    (insert (titlecase-string (delete-and-extract-region begin end)))
    (goto-char pt)))

(defun titlecase-dwim ()
  "Convert the region or current line to title case.
If Transient Mark Mode is on and there is an active region, convert
the region to title case.  Otherwise, work on the current line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (titlecase-region (region-beginning) (region-end))
    (titlecase-region (point-at-bol) (point-at-eol))))


;; ====================================================
;; https://www.emacswiki.org/emacs/SortWords
(defun nv-sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))


;; ====================================================
(defun p-u-p ()
  "Straight update all: pulls and builds if necessary."
  (interactive)
  (progn
    (straight-pull-all)
    (straight-check-all)
    )
  )

;; ====================================================


(defvar nv-diacritics-to-non-diacritics-map
  (cl-map 'list (lambda (a b) (cons a b))
          "ÀÁÂÃÄĀĂàáâãäāăÒÓÔÕÖŌŎòóôõöōŏÈÉÊẼËĒĔèéêẽëēĕÌÍÎĨÏĪĬìíîĩïīĭÙÚÛŨÜŪŬùúûũüūŭÑñ"
          "AAAAAAAaaaaaaaOOOOOOOoooooooEEEEEEEeeeeeeeIIIIIIIiiiiiiiUUUUUUUuuuuuuuNn")
  )

(defun nv-remove-diacritics-from (string)
  "Remove the diacritics from STRING."
  (cl-map 'string (lambda (c) (or (cdr (assoc c nv-diacritics-to-non-diacritics-map)) c)) string))


;; ======================================================================
(defun nv-random-id ()
  "Generates a random alphanumerical XML id: aaa_bbb_ccc."
  (mapconcat
   (lambda (_)
     (mapconcat
      (lambda (_)
        (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
               (random-char (elt alnum (random (length alnum))))
               (result (char-to-string random-char)))
          result))
      (number-sequence 1 3) ""))
   (number-sequence 1 3) "_"))


(defun nv-insert-random-id ()
  (interactive)
  (insert (nv-random-id))
  )


;; ======================================================================

(defun nv-textbf-to-subparagraph (beg end)
  "Replace \\textbf{...} in region with \\subparagraph{...} and add a \\label."
  (interactive "r")
  (cl-flet ((nv-tex-save-slug (text)
              (thread-last text
                           (replace-regexp-in-string "[áà]" "a")
                           (replace-regexp-in-string "[éè]" "e")
                           (replace-regexp-in-string "[íì]" "i")
                           (replace-regexp-in-string "[óò]" "o")
                           (replace-regexp-in-string "[úùü]" "u")
                           (replace-regexp-in-string "[ñ]" "n")
                           (downcase)
                           (replace-regexp-in-string "[^a-z0-9]+" "-")
                           (replace-regexp-in-string "^-\\|-$" ""))))
    (let* ((text (buffer-substring-no-properties beg end))
           (title (and (string-match "\\\\textbf{\\(.*\\)}" text)
                       (match-string 1 text)))
           (slug (nv-tex-save-slug title))
           (replacement (format "\\subparagraph{%s}\n\\label{S:%s}" title slug)))
      (delete-region beg end)
      (insert replacement))))



;; ======================================================================
(defun nv-break-sentences (beg end)
  "Break selected text into one sentence per line, splitting on `.`, `:`, and `;`."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (replace-regexp-in-string
                  "\\([.;:]\\)[[:space:]]+"
                  "\\1\n"
                  (string-trim text))))
    (delete-region beg end)
    (insert result)))


;; ======================================================================
(defun nv-dollar-to-cop (beg end)
  "Replace \\$1.234.567 in region with \\COP{1234567}."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (digits (replace-regexp-in-string "[\\$.]" "" text))
         (replacement (format "\\COP{%s}" (string-trim digits))))
    (delete-region beg end)
    (insert replacement)))



;; ======================================================================
(defun nv-remove-textbf (beg end)
  "Remove \\textbf{...} wrappers in region, leaving their contents."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\textbf{\\([^}]*\\)}" nil t)
      (replace-match "\\1" t))))


;; ======================================================================
(defun nv-latex-table-bold-row ()
  "Convert LaTeX table question lines into bold with midrules.
Removes optional leading ¿ and trailing ? before & \\\\.
Works on region or current line."
  (interactive)
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (narrow-to-region (region-beginning) (region-end))
        (narrow-to-region (line-beginning-position) (line-end-position)))
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*¿?[ \t]*\\([^&?\n][^&\n]*?\\)[ \t]*\\??[ \t]*&[ \t]*\\\\\\\\"
              nil t)
        (replace-match
         "\\\\midrule\n\\\\textbf{\\1} & \\\\\\\\\n\\\\midrule"
         t)))))

;; ======================================================================
(defun nv-latex-subparagraph-from-footnote (beg end)
  "Transform selected LaTeX title+footnote into subparagraph form."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (case-fold-search nil)
         title url)
    (unless (string-match
             "\\`\\s-*\\(.+?\\)%?\\s-*\n\\\\footnote{\\\\url{\\([^}]+\\)}}\\s-*\\'"
             text)
      (user-error "Region does not match expected pattern"))
    (setq title (match-string 1 text))
    (setq url   (match-string 2 text))
    (setq title (string-trim title))
    (delete-region beg end)
    (insert
     (format
      "\\subparagraph
  [%s]
  {%s\\texorpdfstring{\\footnotemark}{}}
\\footnotetext{\\url{%s}}"
      title title url))))

;; ======================================================================
(defun nv-unalign-current ()
  "Remove alignment padding in the current paragraph or active region.
Collapses runs of spaces into a single space."
  (interactive)
  (let* ((beg (if (use-region-p)
                  (region-beginning)
                (save-excursion (backward-paragraph) (point))))
         (end (if (use-region-p)
                  (region-end)
                (save-excursion (forward-paragraph) (point)))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[ \t][ \t]+" end t)
        (replace-match " ")))))

;; ======================================================================


(defun nv-replace-decimal-commas-with-periods (beg end)
  "Replace decimal commas with periods in region or whole buffer.
Prompts for each replacement: yes, no, or all."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((replace-all nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\([0-9]\\),\\([0-9]\\)" end t)
        (if replace-all
            (replace-match "\\1.\\2")
          (let ((choice
                 (read-char-choice
                  (format "Replace %s with '.'? (y=yes n=no a=all) "
                          (match-string 0))
                  '(?y ?n ?a))))
            (cond
             ((eq choice ?y)
              (replace-match "\\1.\\2"))
             ((eq choice ?a)
              (setq replace-all t)
              (replace-match "\\1.\\2"))
             ((eq choice ?n)
              nil))))))))

;; ======================================================================

(defun nv-sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; ======================================================================

(defun nv-latex-wrap-begingroup ()
  "Wrap the active region in \\begingroup ... \\endgroup."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "\n\\endgroup")
        (goto-char beg)
        (insert "\\begingroup\n"))
    (message "No region selected")))

;; ======================================================================
;; ======================================================================
;; ======================================================================


(provide 'myfunctions)
