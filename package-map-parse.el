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
(require 'package-map-secondhelp)

(require 'paren)
(require 'projectile)


(defun package-map-parse--getsourcefiles ()
  "Find all source files from the current project."
  (-filter (lambda (it) (string-suffix-p ".el" it))
           (directory-files (projectile-project-root))))

(defgroup package-map nil
  "Main group for package-map package."
  :group 'coding)

(defcustom package-map-parse-function-shapes
  '((setq . plain) (defvar . plain) (defcustom . plain)
    (defun . note) (defsubst . tab) (defmacro . trapezium))
  "Define variables to look for and graphviz shapes."
  :type 'list
  :group 'package-map)


(defun package-map-parse--alltopdefs-file (file hashdefs)
  "Get all top definitions in FILE and put into HASHDEFS.
Don't use grep or projectile, because those sonuvabitch finish hooks are not reliable."
  (with-current-buffer (find-file-noselect file)
    (goto-char 0)
    (let ((reg-type (package-map-secondhelp--generateregexfromalist package-map-parse-function-shapes)))
      ;;(reg-vnam "\\(-*\\w+\\)+"))
      (while (search-forward-regexp reg-type nil t)
        ;; Get type
        (let* ((type-end (point))
               (type-beg (1+ (move-beginning-of-line 1)))
               (type-nam (buffer-substring-no-properties type-beg type-end)))
          (goto-char type-end)
          (forward-whitespace 1)
          ;; Get variable name
          (let* ((vnam-beg (point))
                 (vnam-end (progn (forward-whitespace 1) (forward-whitespace -1) (point)))
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
                       `(:type ,type-nam
                               :line-beg ,lnum-beg
                               :line-end ,lnum-end
                               :file ,file
                               ;; when mentions is nil, somehow all entries in
                               ;; the hash table point to the same mentions.
                               :mentions (,vnam-nam))
                       hashdefs)))))
      hashdefs)))

(defcustom package-map-parse-hashtablesize 50
  "Size of hash table. 50 by default."
  :group 'package-map)

(defun package-map-parse--alltopdefs-filelist (filelist)
  "Get all top definitions from FILELIST and return a hashtable, with variable names as keys as well as type and bounds as values."
  (let ((hashtable (make-hash-table
                    :size package-map-parse-hashtablesize
                    :test #'equal)))
    (dolist (pfile filelist hashtable)
      (package-map-parse--alltopdefs-file pfile hashtable))))

(defun package-map-parse--allsecondarydefs-file (file hashtable)
  "Get all secondary definitions in FILE for each of the top level definitions in HASHTABLE."
  (let ((funcs-by-line-asc (package-map-secondhelp--makesortedlinelist
                            hashtable)))
    ;; -- Check each top def in the buffer
    (with-current-buffer (find-file-noselect file)
      (maphash   ;; iterate hashtable
       (lambda (vname annotations)
         (package-map-secondhelp--updatementionslist vname
                                                     annotations
                                                     funcs-by-line-asc))
       hashtable))))

(defun package-map-parse--allsecondarydefs-filelist (filelist hashtable)
  "Get all secondary definitions for all files in FILELIST for the top level definitions in HASHTABLE."
  (dolist (pfile filelist hashtable)
    (package-map-parse--allsecondarydefs-file pfile hashtable)))

(defun package-map-parse--generatemap ()
  "Generate a map of toplevel function and variable definitions in a project."
  (let* ((proj-files (package-map-parse--getsourcefiles))
         (hash-table (package-map-parse--alltopdefs-filelist proj-files)))
    (package-map-parse--allsecondarydefs-filelist proj-files hash-table)
    hash-table))

(provide 'package-map-parse)
;;; package-map-parse.el ends here
