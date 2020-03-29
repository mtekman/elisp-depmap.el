;;; package-map-secondhelp.el --- Helper functions for parse library -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/remind-bindings.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (projectile "2.2.0-snapshot"))
;; Version: 0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; See package-map.el

;;; Code:
(require 'cl-lib)
(require 'dash)

(defgroup package-map nil
  "Main group for package-map package."
  :group 'coding)

(defsubst package-map-secondhelp--generateregexfromalist (alist)
  "From ALIST, get the car variables and put them in a regex.
This will be used to scan all files for top level definitions."
  (concat "^(\\("
          (mapconcat (lambda (x) (format "%s" (car x)))
                     alist "\\|") "\\)"))


(defun package-map-secondhelp--callingfuncatline (lnum file list-asc)
  "Retrieve the function name in LIST-ASC that LNUM bisects in FILE."
  (let ((func nil))
    (dolist (elm list-asc)
      (let ((func-lbeg (1+ (nth 0 elm)))
            (func-lend (nth 1 elm))
            (func-name (nth 2 elm))
            (func-file (nth 3 elm)))
        ;; If current line number in file matches the
        ;; flanking beg and end in the list (and the file)
        (if (and (string= file func-file)
                 (<= func-lbeg lnum func-lend))
            (cl-pushnew func-name func))))
    (if func
        (if (> 1 (length func))
            (error "Multiple functions at line... %d: %s" lnum func)
          (car func)))))

(defun package-map-secondhelp--makesortedlinelist (hashtable)
  "Make an ascending list of the start and end positions of all functions from HASHTABLE."
  (let ((funcsbylinenum nil))
    (maphash
     (lambda (nam vals)
       (let ((lbeg (plist-get vals :line-beg))
             (lend (plist-get vals :line-end))
             (file (plist-get vals :file)))
         ;; We only want functions (those with a lend)
         (if lend
             (cl-pushnew `(,lbeg ,lend ,nam ,file) funcsbylinenum))))
     hashtable)
    (--sort (< (car it) (car other)) funcsbylinenum)))

(defun package-map-secondhelp--updatementionslist (vname file annotations funcs-by-line-asc)
  "Update mentions list from ANNOTATIONS for variable VNAME by checking in ASCLIST of line numbers for function bounds in FILE."
  (save-excursion
    (let ((vnam-regex (format "\\( \\|(\\|\\b\\)%s\\( \\|)\\|\\b\\)" vname))
          (mentionlst (plist-get annotations :mentions))
          (vnam-line (plist-get annotations :line-beg)))
      (goto-char 0)
      (while (search-forward-regexp vnam-regex nil t)
        (let ((lnum (line-number-at-pos)))
          (unless (eq lnum vnam-line)
            ;; skip the top level definition
            (let ((called-func (package-map-secondhelp--callingfuncatline
                                lnum
                                file
                                funcs-by-line-asc)))
              (if called-func
                  (push called-func mentionlst))))))
      (plist-put annotations :mentions mentionlst))))

(provide 'package-map-secondhelp)
;;; package-map-secondhelp.el ends here
