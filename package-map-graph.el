;;; package-map-graph.el --- Generate a graphviz map of functions and definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

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
(require 'package-map-parse)
(require 'subr-x)

(defcustom package-map-graph-stripprojectname t
  "Strip the project name from the graph."
  :type 'boolean
  :group 'package-map)

(defcustom package-map-graph-linemod 10
  "Line scaling modifier.  Higher reduces the border width."
  :type 'integer
  :group 'package-map)

(defvar package-map-graph--colors-available
  '(red blue darkgreen orange purple gray green yellow pink brown navy maroon violet))

(defvar package-map-graph--symbols-available
  '("ᚻ" "ᛉ" "ᛊ" "ᛋ" "ᛗ" "ᛝ" "ᛢ" "ᛪ" "ᛯ" "ᛸ" "ᛒ" "ᚷ" "ᚫ" "ᚣ" "ŧ" "Ω" "Æ" "þ"))

(defun package-map-graph--filesuniq (hashtable)
  "Get the unique files in HASHTABLE."
  (seq-uniq (--map (plist-get it :file)
                   (hash-table-values hashtable))))

(defun package-map-graph--makefilemapcolors (hashtable)
  "From the HASHTABLE make a plist of file, cluster no, and color for each file."
  (let ((colors package-map-graph--colors-available)
        (symbls package-map-graph--symbols-available)
        (files-uniq (package-map-graph--filesuniq hashtable)))
    (--map (let ((colr (nth it colors))
                 (file (nth it files-uniq))
                 (symb (nth it symbls))
                 (clst (format "cluster_%d" it)))
             `(:file ,file :color ,colr :clust ,clst, :symbol ,symb))
           (number-sequence 0 (1- (length files-uniq))))))

(defun package-map-graph--newname (functionname &optional filename symbol)
  "Strip the projectname from FUNCTIONNAME, or use the FILENAME as the prefix to strip off.  If SYMBOL, use that as replacement."
  (let* ((proot (or filename (projectile-project-name (projectile-project-root))))
         (prool (car (split-string proot "\\.el")))
         (pregx (format "^%s" prool)))
    (if package-map-graph-stripprojectname
        (replace-regexp-in-string pregx (or symbol "§") functionname)
      functionname)))

(defcustom package-map-graph-decoratesubgraph
  '((style . rounded) (bgcolor . white) (fontsize . 25.0) (labelfloat . true) (fontname . "\"times bold\""))
  "Attributes to decorate subgraph with."
  :type 'alist
  :group 'package-map)

(defun package-map-graph--decorate-subgraph ()
  "Generate format string for `package-map-graph-decoratesubgraph'."
  (mapconcat (lambda (x) (format "      %s=%s;" (car x) (cdr x)))
             package-map-graph-decoratesubgraph
             "\n"))

(defun package-map-graph--makedigraphgroups (hashtable filemap funcmap &optional noclust)
  "Make digraph subgraphs for each file cluster, using files from HASHTABLE.
Decorate them using colors from FILEMAP and shapes from FUNCMAP.
If NOCLUST, do not cluster functions from the same file."
  (dolist (vfile (--map (plist-get it :file) filemap))
    (let* ((entry (--first (string= (plist-get it :file) vfile) filemap))
           (color (plist-get entry :color))
           (clust (plist-get entry :clust))
           (symbl (plist-get entry :symbol)))
      (insert (format "  subgraph %s {\n" (if noclust
                                              (format "\"%s\"" vfile)
                                            clust))
              (format "%s\n" (package-map-graph--decorate-subgraph))
              (format "      label = \"%s\";\n" (if package-map-graph-stripprojectname
                                                    (format "[%s] %s" symbl vfile)
                                                  vfile))
              (format "      edge [color=%s];\n" color)
              (format "      node [color=%s];\n" color))
      ;; First pass define nodes
      (let ((customs '("rank = same")))
        (maphash
         (lambda (funcname info)
           ;; Only process functions from VFILE
           (if (string= (plist-get info :file) vfile)
               (let ((oname (package-map-graph--newname funcname vfile symbl))
                     (vbegs (plist-get info :line-beg))
                     (vends (plist-get info :line-end))
                     (vtype (plist-get info :type)))
                 (if (string= "defcustom" vtype)
                     ;; Store customs to use as same rank later
                     (cl-pushnew (format "\"%s\"" oname) customs))
                 (let ((numlines (if vends (- vends vbegs) 1)))
                   (insert (format "      node [shape=%s,penwidth=%s] \"%s\";\n"
                                   (alist-get (intern vtype) funcmap)
                                   (1+ (/ numlines package-map-graph-linemod))
                                   oname))))))
         hashtable))
      ;; Rank all customs at the top -- looks ugly, leave it.
      ;; (insert (format "      {%s;}\n" (mapconcat 'identity
      ;;                                            (reverse customs) ";"))))
      ;; Second pass define intrafile links
      (maphash
       (lambda (funcname info)
         ;; Only process functions from VFILE
         (let ((oname (package-map-graph--newname funcname vfile symbl))
               (vment (plist-get info :mentions)))
           (if (eq (plist-get info :file) vfile)
               (dolist (mento vment)
                 (unless (eq funcname mento)
                   (let* ((mento-info (gethash mento hashtable))
                          (mento-file (plist-get mento-info :file)))
                     ;; If functions are from the same file,
                     ;; list them here.
                     (if (string= vfile mento-file)
                         (insert (format "      \"%s\" -> \"%s\";\n"
                                         oname
                                         (package-map-graph--newname mento
                                                                     vfile
                                                                     symbl))))))))))
       hashtable))
    (insert "  }\n")))


(defun package-map-graph--makedigraphcrossinglinks (hashtable filemap)
  "Make the digraph connections across clusters, using functions from HASHTABLE, and FILEMAP info."
  (maphash
   (lambda (funcname info)
     (let ((vfile (plist-get info :file))
           (vment (plist-get info :mentions)))
       (let* ((ventry (--first (string= (plist-get it :file) vfile) filemap))
              (vsymbl (plist-get ventry :symbol))
              (oname (package-map-graph--newname funcname vfile vsymbl)))
         (dolist (mento vment)
           (unless (eq funcname mento)
             (let* ((mento-info (gethash mento hashtable))
                    (mento-file (plist-get mento-info :file))
                    (mento-entr (--first (string= (plist-get it :file) mento-file)
                                         filemap))
                    (mento-symb (plist-get mento-entr :symbol)))
               ;; If functions are NOT from the same file,
               ;; list them here.
               (unless (string= vfile mento-file)
                 (insert (format
                          "  \"%s\" -> \"%s\";\n"
                          oname
                          (package-map-graph--newname mento
                                                      mento-file
                                                      mento-symb))))))))))
   hashtable))


(provide 'package-map-graph)
;;; package-map-graph.el ends here
