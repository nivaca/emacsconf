;;; turbo-c-nv-theme.el --- Emacs theme matching Turbo C 3.0 VSCode theme -*- lexical-binding: t; -*-

;;; Code:

(deftheme turbo-c-nv
  "Turbo C 3.0 / Borland style theme faithfully matching the JSON theme.")

(let* (;; Exact palette from the JSON theme
       (tc-blue        "#0000A8")
       (tc-dark-blue   "#000080")
       (tc-bright-blue "#0000FF")
       (tc-yellow      "#FFFF00")
       (tc-dark-yellow "#808000")
       (tc-white       "#FFFFFF")
       (tc-light-gray  "#D3D3D3")
       (tc-dark-gray   "#808080")
       (tc-cyan        "#00FFFF")
       (tc-dark-cyan   "#00AAAA")
       (tc-green       "#00FF00")
       (tc-dark-green  "#00AA00")
       (tc-red         "#EE0000")
       (tc-dark-red    "#800000")
       (tc-magenta     "#FF00FF")
       (tc-pink        "#FF69B4")
       (tc-black       "#000000")

       ;; UI colors
       (tc-menu-bg     "#D3D3D3")
       (tc-menu-fg     "#000000")
       (tc-modeline-bg "#D3D3D3")
       (tc-modeline-fg "#000000")
       (tc-region-bg   "#808080"))

  (custom-theme-set-faces
   'turbo-c-nv

   ;; ===== Basic faces =====
   `(default ((t (:background ,tc-blue :foreground ,tc-green))))
   `(cursor ((t (:background ,tc-white :foreground ,tc-blue))))
   `(region ((t (:background ,tc-region-bg :foreground ,tc-white))))
   `(highlight ((t (:background ,tc-dark-cyan :foreground ,tc-white))))
   `(secondary-selection ((t (:background ,tc-dark-red :foreground ,tc-white))))
   `(fringe ((t (:background ,tc-blue :foreground ,tc-light-gray))))
   `(vertical-border ((t (:foreground ,tc-light-gray :background ,tc-blue))))
   `(minibuffer-prompt ((t (:foreground ,tc-white :weight bold))))
   `(error ((t (:foreground ,tc-red :weight bold))))
   `(warning ((t (:foreground ,tc-yellow :weight bold))))
   `(success ((t (:foreground ,tc-green :weight bold))))
   `(shadow ((t (:foreground ,tc-dark-gray))))
   `(link ((t (:foreground ,tc-cyan :underline t))))
   `(trailing-whitespace ((t (:background ,tc-dark-red))))

   ;; ===== Menu / Tabs =====
   `(menu ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg))))
   `(tool-bar ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg))))
   `(header-line ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg :weight bold))))

   ;; ===== Mode line =====
   `(mode-line ((t (:background ,tc-modeline-bg
                    :foreground ,tc-modeline-fg
                    :box (:line-width 1 :color ,tc-black)))))
   `(mode-line-inactive ((t (:background ,tc-black
                             :foreground ,tc-dark-gray
                             :box (:line-width 1 :color ,tc-black)))))

   ;; ===== Current line =====
   `(hl-line ((t (:background ,tc-dark-cyan))))
   `(line-number ((t (:background ,tc-blue :foreground ,tc-light-gray))))
   `(line-number-current-line ((t (:background ,tc-blue :foreground ,tc-white :weight bold))))

   ;; ===== Syntax highlighting =====
   `(font-lock-builtin-face ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,tc-cyan :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,tc-cyan :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,tc-cyan :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,tc-green :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,tc-bright-blue :weight bold))))
   `(font-lock-string-face ((t (:foreground ,tc-red))))
   `(font-lock-type-face ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,tc-dark-green))))
   `(font-lock-warning-face ((t (:foreground ,tc-red :weight bold))))

   ;; ===== Search =====
   `(isearch ((t (:background ,tc-dark-cyan
                   :foreground ,tc-white
                   :weight bold))))
   `(lazy-highlight ((t (:background ,tc-dark-gray
                         :foreground ,tc-white))))

   ;; ===== Org =====
   `(org-level-1 ((t (:foreground ,tc-white :weight bold))))
   `(org-level-2 ((t (:foreground ,tc-yellow :weight bold))))
   `(org-level-3 ((t (:foreground ,tc-cyan :weight bold))))
   `(org-link ((t (:foreground ,tc-cyan :underline t))))

   ;; ===== Dired =====
   `(dired-directory ((t (:foreground ,tc-white :weight bold))))
   `(dired-symlink ((t (:foreground ,tc-cyan))))

   ;; ===== Diff =====
   `(diff-added ((t (:foreground ,tc-green :background ,tc-dark-green))))
   `(diff-removed ((t (:foreground ,tc-red :background ,tc-dark-red))))

   ;; ===== Magit =====
   `(magit-section-heading ((t (:foreground ,tc-white :weight bold))))
   `(magit-branch-local ((t (:foreground ,tc-cyan :weight bold))))
   `(magit-branch-remote ((t (:foreground ,tc-green :weight bold))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'turbo-c-nv)

;;; turbo-c-nv-theme.el ends here
