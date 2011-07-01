;; find-companion-thing.el - locate and visit a companion file
;;
;; Purpose: 
;;
;; Locate and visit a companion to the currently visited file.  Your
;; file has a companion when its filename differs in a predictable way
;; and is located in a predictable place.  If you want to quickly
;; switch between companions, this function might be for you.  As
;; written it supports C or C++, where you have .h vs .c, .c++, or
;; .cpp located in "include", "inc" and "src" or "source" sibling
;; directories. There is one function to call: fct/find-file.  It
;; toggles between the include and the source and the header, as
;; applicable. 
;;
;; Usage:
;;
;;     (autoload 'fct/find-file "find-companion-thing" nil t)
;;     (global-set-key "\C-x\C-h" 'fct/find-file)
;;
;; Customization: See defvars.
;;
;; License:
;;
;; Copyright (C) 2011 Mitchell Perilstein -- arraybound@gmail.com
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from 
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

(defvar fct/c-exts '("c" "cpp" "cc" "cxx")
  "C-like file extensions")

(defvar fct/h-exts '("h")
  "C-like header file extensions")

(defvar fct/c-dirs '("." "../src" "../source")
  "C-like source file locations")

(defvar fct/h-dirs '("."  "../inc" "../include")
  "C-like header file locations")

(defun fct/find-file ()
  "Visit the companion of the currently visited file.  Toggles between
companion files."
  (interactive)
  (let* ((fname (buffer-file-name))	; /a/b/base.ext
	 (found (fct/find-companion-thing 
		 (file-name-sans-extension (file-name-nondirectory fname)) ; "base"
		 (file-name-extension fname))))	  ; "ext"
    (if found 
	(find-file found))))

(defun fct/find-companion-thing (base ext)
  (cond ((member ext fct/c-exts)
	 (fct/filter 'fct/try-file (fct/cross-product fct/h-dirs fct/h-exts)))
	((member ext fct/h-exts)
	 (fct/filter 'fct/try-file (fct/cross-product fct/c-dirs fct/c-exts)))
	(t (progn 
	     (error "I don't know the %s file type" ext)
	     nil))))

(defun fct/try-file (pair)
  (let* ((dir (car pair))
	 (ext (cadr pair))
	 (file (concat dir "/" base "." ext)))
    (if (file-exists-p file) file nil)))

;;
;; short circuit - stop traversing when first true test is hit
;;
(defun fct/filter (test xs)
  (if (null xs) 
      nil
    (or (funcall test (car xs))
	(fct/filter test (cdr xs)))))

;;
;; (cross-product '(a b c) '(1 2 3))
;; ((a 1) (a 2) (a 3) (b 1) (b 2) (b 3) (c 1) (c 2) (c 3))
;;
(defun fct/cross-product (xs ys)
  (apply 'append (mapcar 
		  (lambda (x) 
		    (mapcar 
		     (lambda (y) (list x y))
		     ys))
		  xs)))

;;; end