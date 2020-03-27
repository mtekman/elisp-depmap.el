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

;; helper funcs

(defcustom package-map-graph-stripprojectname t
  "Strip the project name from the graph."
  :type 'boolean
  :group 'package-map)

(defun package-map-graph--filesuniq (hashtable)
  "Get the unique files in HASHTABLE."
  (seq-uniq (--map (plist-get it :file)
                   (hash-table-values hashtable))))

(defvar package-map-graph--colors-available
  '(red blue green orange purple gray yellow pink brown navy maroon violet))

(defun package-map-graph--makefilemapcolors (hashtable)
  "From the HASHTABLE make a plist of file, cluster no, and color for each file."
  (let ((colors package-map-graph--colors-available)
        (files-uniq (package-map-graph--filesuniq hashtable)))
    (--map (let ((colr (nth it colors))
                 (file (nth it files-uniq))
                 (clst (format "cluster_%d" it)))
             `(:file ,file :color ,colr :clust ,clst))
           (number-sequence 0 (1- (length files-uniq))))))

(defun package-map-graph--newname (functionname)
  "Strip the projectname from FUNCTIONNAME."
  (let* ((proot (projectile-project-name (projectile-project-root)))
         ;; FIXME: (projectile-project-root) is nil when called within anonymous function
         (prool (car (split-string proot ".el")))
         (pregx (format "^%s-" prool)))
    (if package-map-graph-stripprojectname
        (replace-regexp-in-string pregx "§" functionname)
      functionname)))

(defun package-map-graph--makedigraphgroups (hashtable filemap funcmap &optional surround)
  "Make digraph subgraphs for each file cluster, using files from HASHTABLE.
Decorate them using colors from FILEMAP and shapes from FUNCMAP.
If SURROUND, box the functions in each file."
  (dolist (vfile (--map (plist-get it :file) filemap))
    (let* ((entry (--first (string= (plist-get it :file) vfile) filemap))
           (color (plist-get entry :color))
           (clust (plist-get entry :clust)))
      (insert (format "  subgraph %s {\n" (if surround clust (format "\"%s\"" vfile))))
      (insert (format "      node [color=%s];\n" color))
      (insert (format "      label = \"%s\";\n" vfile))
      (insert (format "      color=black;\n")))
    ;; First pass define nodes
    (maphash
     (lambda (funcname info)
       ;; Only process functions from VFILE
       (if (eq (plist-get info :file) vfile)
           (let ((oname (package-map-graph--newname funcname))
                 (vbegs (plist-get info :line-beg))
                 (vends (plist-get info :line-end))
                 (vtype (plist-get info :type)))
             (let ((numlines (if vends (- vends vbegs) 1)))
               (insert (format "      node [shape=%s,penwidth=%s] \"%s\";\n"
                               (alist-get (intern vtype) funcmap)
                               (1+ (/ numlines 5))
                               oname))))))
     hashtable)
    ;; Second pass define intrafile links
    (maphash
     (lambda (funcname info)
       ;; Only process functions from VFILE
       (let ((oname (package-map-graph--newname funcname))
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
                                       (package-map-graph--newname mento))))))))))
     hashtable)
    (insert "  }\n")))

(defun package-map-graph--makedigraphcrossinglinks (hashtable)
  "Make the digraph connections across clusters, using functions from HASHTABLE."
  (maphash
   (lambda (funcname info)
     (let ((oname (package-map-graph--newname funcname))
           (vfile (plist-get info :file))
           (vment (plist-get info :mentions)))
       (dolist (mento vment)
         (unless (eq funcname mento)
           (let* ((mento-info (gethash mento hashtable))
                  (mento-file (plist-get mento-info :file)))
             ;; If functions are NOT from the same file,
             ;; list them here.
             (unless (string= vfile mento-file)
               (insert (format "  \"%s\" -> \"%s\"\n"
                               oname
                               (package-map-graph--newname mento)))))))))
   hashtable))


(provide 'package-map-graph)
;;; package-map-graph.el ends here
