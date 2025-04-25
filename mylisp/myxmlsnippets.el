;;; mylisp/myxmlsnippets.el -*- lexical-binding: t; -*-

(use-package transient
  :straight t)

(defvar allwits "#C #A #B #D #E #F #G #H #I")

(transient-define-prefix nv-xml-snippets ()
  "Main XML Prefix Menu"
  ["Insert:"
   ("a"
    "appartus entry"
    nv-xml-apparatus-prefixes)

   ("s"
    "search snippet"
    (lambda () (interactive)
      (yas-insert-snippet))
    :transient nil)

   ("w"
    "insert all other witnesses"
    (lambda () (interactive)
      (insert allwits)
      )
    :transient nil)

   ("t"
    "wrap element in tags"
    (lambda () (interactive)
      (nv-wrap-element-with-tag ())
      )
    :transient nil)

   ]
  )


(transient-define-prefix nv-xml-apparatus-prefixes ()
  "XML entries for critical apparatus."
  [
   ["Insert apparatus:"
    ("a"
     "normal apparatus"
     (lambda () (interactive)
       (yas-expand-snippet (yas-lookup-snippet "app-entry"))
       )
     :transient nil)
    
    ("e"
     "empty lemma"
     (lambda () (interactive)
       (yas-expand-snippet (yas-lookup-snippet "app-empty-lem"))
       )
     :transient nil)
    
    ("r"
     "empty reading"
     (lambda () (interactive)
       (yas-expand-snippet (yas-lookup-snippet "app-empty-rdg"))
       )
     :transient nil)
    ]

   

   [[""] ;;empty line

    ("i"
     "variation-inversion"
     (lambda () (interactive)
       (yas-expand-snippet (yas-lookup-snippet "app-inversion"))
       )
     :transient nil)
    
    ("o"
     "variation-orthography"
     (lambda () (interactive)
       (yas-expand-snippet (yas-lookup-snippet "app-var-ort"))
       )
     :transient nil)

    ("t"
     "variation-orthography: t/c"
     (lambda () (interactive)
       (yas-expand-snippet (yas-lookup-snippet "app-var-ort-t/c"))
       )
     :transient nil)]
   
   ]
  )


(provide 'myxmlsnippets)
