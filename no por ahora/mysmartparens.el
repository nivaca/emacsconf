  ;; LaTeX modes
  ;; (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
  ;;   (sp-local-pair "$" "$")
  ;;   (sp-local-pair "\[" "\]")
  ;;   (sp-local-pair "\{" "\}")
  ;;   (sp-local-pair "‘" "’")
  ;;   (sp-local-pair "“" "”")
  ;;   (sp-local-pair "\\begin" "\\end")
  ;;   (sp-local-tag "i" "\"<" "\">")
  ;;   ;; (sp-local-pair "$" nil :unless '(sp-point-before-word-p))
  ;;   ;; (sp-local-pair "(" ")" :unless '(sp-point-before-word-p))
  ;;   ;; (sp-local-pair "[" "]" :unless '(sp-point-before-word-p))
  ;;   ;; (sp-local-pair "{" "}" :unless '(sp-point-before-word-p))
  ;;   )



;;; org-mode
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»")
    (sp-local-pair "`" "'" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    )


;;; markdown-mode
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))


  ;; Text modes
  (sp-with-modes '(text-mode)
    ;; math modes, yay. The :actions are provided automatically if
    ;; these pairs do not have global definition.
    (sp-local-pair "\[" "\]")
    (sp-local-pair "\{" "\}")
    (sp-local-pair "‘" "’")
    (sp-local-pair "“" "”")
    )