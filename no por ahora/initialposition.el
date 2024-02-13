
;; -------------------------------------------------------------------
;; Initial position of main frame
(use-package emacs
  :config
  (defun nv-set-frame-position ()
    (interactive)
    (when window-system  ;; not in console
      (set-frame-position (selected-frame) 0 0)
      (pcase (system-name)
        ;; PC escritorio casa
        ("nivaca-pc"
         (defcustom nv-screen-size '(1920 1080)
           "Screen size in pixels"))
        ;; XPS 13
        ("nivaca-xps"
         (defcustom nv-screen-size '(1920 1200)
           "Screen size in pixels"))
        ;; TP
        ("nivaca-tp"
         (defcustom nv-screen-size '(1920 1200)
           "Screen size in pixels"))
        ) ;; end: pcase
      ;; Mac
      (when IS-MAC
        (defcustom nv-screen-size '(1920 1200)
          "Screen size in pixels"
          )
        )
      (setq frame-resize-pixelwise t)
      (set-frame-position (selected-frame) 0 0)
      (let ((dx -2)  ;; added to adjust width
            (dy +2)) ;; added to adjust height
        (set-frame-width
         (selected-frame) 
         (+
          (/
           (/ (nth 0 nv-screen-size) 2)  ;; half of screen width
           (frame-char-width))
          dx)
         nil)
        (set-frame-height
         (selected-frame)
         (+
          (/
           (nth 1 nv-screen-size)
           (frame-char-height))
          dy)
         nil)
        ) ; let
      ) ; when
    ) ; defun
  ;; call the function now:
  ;; (nv-set-frame-position)
  )
