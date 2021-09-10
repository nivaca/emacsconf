;;; spanish.el --- Setup AUCTeX for editing Spanish text.

;; Copyright (C) 2004, 2005, 2018 Free Software Foundation, Inc.

;; Author: Nicolas Vaughan <nivaca@fastmail.com>
;; Maintainer: Nicolas Vaughan <nivaca@fastmail.com>
;; Created: 2020-08-19
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; However, I will be glad to see a normative reference. -- DGMS

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-quotes
		  "font-latex"
		  (quotes))

(defvar TeX-language-es-hook nil
  "Hook run for Spanish texts.")

(TeX-add-style-hook
 "spanish"
 (lambda ()
   ;; XXX: Handle former customizations of the now defunct
   ;; Spanish-specific variables.  References to the respective
   ;; variables are to be deleted in future versions. (now = 2005-04-01)
   (unless (eq (car TeX-quote-language) 'override)
     (let ((open-quote (if (and (boundp 'LaTeX-spanish-open-quote)
				LaTeX-spanish-open-quote)
			   LaTeX-spanish-open-quote
			 "\\enquote{"))
	   (close-quote (if (and (boundp 'LaTeX-spanish-close-quote)
				 LaTeX-spanish-close-quote)
			    LaTeX-spanish-close-quote
			  "}")))
       (setq TeX-quote-language
	     `("spanish", open-quote, close-quote, TeX-quote-after-quote))))
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\enquote{" "}" spanish)))
   (run-hooks 'TeX-language-es-hook))
 LaTeX-dialect)

;;; spanish.el ends here
