;;; package-map-parse.el --- Uses projectile to construct a hashtable of definitions -*- lexical-binding: t; -*-

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

;; Go through all files using projectile and retrieve top level
;; definitions and their positions then determine which functions
;; are called by which others.


;;; Code:
(require 'paren)
(provide 'package-map-secondhelp)

(defun getsourcefiles ()
  "Find all source files from the current project."
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-project-files project-root)))
    (--filter (string-suffix-p ".el" it) project-files)))

(defun alltopdefs-file (file hashdefs)
  "Get all top definitions in FILE and put into HASHDEFS.
Don't use grep or projectile, because those sonuvabitch finish hooks are not reliable."
  (with-current-buffer (find-file-noselect file)
    (goto-char 0)
    (let ((reg-type "^(\\(setq\\|\\(def\\(un\\|var\\|subst\\|custom\\)\\)\\)")
          (reg-vnam "\\(-?\\w+\\)+"))
      (while (search-forward-regexp reg-type nil t)
        ;; Get type
        (let* ((type-end (point))
               (type-beg (1+ (move-beginning-of-line 1)))
               (type-nam (buffer-substring-no-properties type-beg type-end)))
          (goto-char type-end)
          (forward-whitespace 1)
          ;; Get variable name
          (let* ((vnam-beg (point))
                 (vnam-end (search-forward-regexp reg-vnam))
                 (vnam-nam (buffer-substring-no-properties vnam-beg vnam-end)))
            ;; Get bounds or line number
            (let ((lnum-beg (line-number-at-pos))
                  (lnum-end nil))
              (when (string= type-nam "defun")
                (move-beginning-of-line 1)
                (let* ((bounk (funcall show-paren-data-function))
                       (keybl (nth 3 bounk)))
                  (goto-char keybl)
                  (setq lnum-end (line-number-at-pos))))
              (puthash vnam-nam
                       `(:type ,type-nam :line-beg ,lnum-beg :line-end ,lnum-end :file ,file :mentions nil)
                       hashdefs)))))
      hashdefs)))

(defun alltopdefs-filelist (filelist)
  "Get all top definitions from FILELIST and return a hashtable, with variable names as keys as well as type and bounds as values."
  (let ((hashtable (make-hash-table :size 1000)))
    (dolist (pfile filelist hashtable)
      (alltopdefs-file pfile hashtable))))

(defun allsecondarydefs-file (file hashtable)
  "Get all secondary definitions in FILE for each of the top level definitions in HASHTABLE."
  (let ((funcs-by-line-asc (makesortedlinelist hashtable)))
    ;; -- Check each top def in the buffer
    (with-current-buffer (find-file-noselect file)
      (maphash   ;; iterate hashtable
       (lambda (vname annotations)
         (updatementionslist vname annotations funcs-by-line-asc))
       hashtable))))

(defun allsecondarydefs-filelist (filelist hashtable)
  "Get all secondary definitions for all files in FILELIST for the top level definitions in HASHTABLE."
  (dolist (pfile filelist hashtable)
    (allsecondarydefs-file pfile hashtable)))

(defun generatemap ()
  "Generate a map of toplevel function and variable definitions in a project."
  (let* ((proj-files (getsourcefiles))
         (hash-table (alltopdefs-filelist proj-files)))
    (allsecondarydefs-filelist proj-files hash-table)
    hash-table))

(provide 'package-map-parse)
;;; package-map-parse.el ends here
