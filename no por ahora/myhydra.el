;;; myhydra.el --- My hydra bindings

;;; Commentary:

;;; Code:

(use-package hydra
  :ensure t
  )

(defhydra hydra-transpose (:color red)
  "Transpose"
  ("c" transpose-chars "characters")
  ("w" transpose-words "words")
  ("o" org-transpose-words "Org mode words")
  ("l" transpose-lines "lines")
  ("s" transpose-sentences "sentences")
  ("e" org-transpose-elements "Org mode elements")
  ("p" transpose-paragraphs "paragraphs")
  ("t" org-table-transpose-table-at-point "Org mode table")
  ("q" nil "cancel" :color blue))

;; (defhydra hydra-move-text (:color red)
;;   "Move text."
;;   ("<right>" nv/shift-right)
;;   ("<left>" nv/shift-left)
;;   ("q" nil "cancel" :color blue))



(provide 'myhydra)

;;; myhydra.el ends here
