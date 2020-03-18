;;; package-map-secondhelp.el --- Helper functions to second pass -*- lexical-binding: t; -*-

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


;;; Code:
(require 'cl-lib)

(defun calling-func-atline (lnum list-asc)
  "Retrieve the function name in LIST-ASC at LNUM bisects."
  (let ((func nil))
    (dolist (elm list-asc)
      (let ((func-lbeg (1+ (nth 0 elm)))
            (func-lend (nth 1 elm))
            (func-name (nth 2 elm)))
        (if (<= func-lbeg lnum func-lend)
            (cl-pushnew func-name func))))
    (if func
        (if (> 1 (length func))
            (error "Multiple functions at line... %d: %s" lnum func)
          (car func)))))

(defun makesortedlinelist (hashtable)
  "Make an ascending list of the start and end positions of all functions from HASHTABLE."
  (let ((funcsbylinenum nil))
    (maphash
     (lambda (nam vals)
       (let ((lbeg (plist-get vals :line-beg))
             (lend (plist-get vals :line-end)))
         ;; We only want functions (those with a lend)
         (if lend
             (cl-pushnew `(,lbeg ,lend ,nam) funcsbylinenum))))
     hashtable)
    (--sort (< (car it) (car other)) funcsbylinenum)))

(defun updatementionslist (vname annotations asclist)
  "Update mentions list from ANNOTATIONS for variable VNAME by checking in ASCLIST of line numbers for function bounds."
  (let ((vnam-regex (format "\\( \\|(\\)%s\\( \\|)\\)" vname))
        (mentionlst (plist-get annotations :mentions))
        (vnam-line (plist-get annotations :line-beg)))
    (goto-char 0)
    (while (search-forward-regexp vnam-regex nil t)
      (let ((lnum (line-number-at-pos)))
        (unless (eq lnum vnam-line)
          (let ((called-func (calling-func-atline
                              lnum
                              asclist)))
            (if called-func
                (cl-pushnew called-func mentionlst))))))
    ;; swap updated mentionlst
    (plist-put annotations :mentions mentionlst)))

(provide 'package-map-secondhelp)
;;; package-map-secondhelp.el ends here
