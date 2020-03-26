;;; package-map.el --- Generate a graphviz map of functions and definitions -*- lexical-binding: t; -*-

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

;; TODO: - Label (interactive) when parsing.

;;; Code:
(require 'package-map-parse)
(require 'org-table)
(require 'subr-x)

(defun package-map-makesummarytable ()
  "Make a summary org table of variables and references to them."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    (with-current-buffer (find-file "graphviz2.org")
      (erase-buffer)
      (insert "| Type | Name | File | #Lines |\
 #Mentions | Mentions |\n|--\n")
      (maphash
       (lambda (funcname info)
         (let ((vfile (plist-get info :file))
               (vbegs (plist-get info :line-beg))
               (vends (plist-get info :line-end))
               (vtype (plist-get info :type))
               (vment (plist-get info :mentions)))
           (insert
            (format "| %s | %s | %s | %d | %d | %s |\n"
                    vtype funcname vfile
                    (if vends (- vends vbegs) 1)
                    (length vment)
                    vment))))
       hashtable)
      (org-table-align))))


(defvar package-map-colors-available
  '(red blue green orange purple gray yellow pink brown navy maroon violet))


(defun package-map--filesuniq (hashtable)
  "Get the unique files in HASHTABLE."
  (seq-uniq (--map (plist-get it :file)
                   (hash-table-values
                    hashtable))))

(defun package-map--makecolormap (hashtable)
  "From the HASHTABLE make a color map of files."
  (let ((colors package-map-colors-available)
        (files-uniq (package-map--filesuniq hashtable)))
    (--map (let ((colr (nth it colors))
                 (file (nth it files-uniq)))
             `(,file . ,colr))
           (number-sequence 0 (1- (length files-uniq))))))

(defcustom package-map-stripprojectname t
  "Strip the project name from the graph."
  :type 'boolean
  :group 'package-map)

(defun package-map--newname (fname)
  "Strip the projectname from FNAME."
  (let* ((proot (projectile-project-name (projectile-project-root)))
         (prool (car (split-string proot ".el")))
         (pregx (format "^%s-" prool)))
    (if package-map-stripprojectname
        (replace-regexp-in-string pregx "§" fname)
      fname)))


(defun package-map-makedotfile ()
  "Make a dot file representation of all the top level definitions in a project, and their references."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    ;; TODO: implement these
    (let ((colormap (package-map--makecolormap hashtable))
          (shapemap package-map-parse-function-shapes))
      (with-current-buffer (find-file-noselect "graphviz2.dot")
        (erase-buffer)
        (insert "strict graph {\n")
        (maphash
         (lambda (funcname info)
           (let ((oname (package-map--newname funcname))
                 (vfile (plist-get info :file))
                 (vbegs (plist-get info :line-beg))
                 (vends (plist-get info :line-end))
                 (vtype (plist-get info :type))
                 (vment (plist-get info :mentions)))
             (let ((numlines (if vends (- vends vbegs) 1)))
               (insert
                (format "  \"%s\" [shape=%s,color=%s,=%s]\n"
                        oname
                        (alist-get (intern vtype) shapemap)
                        (alist-get vfile colormap)
                        (1+ (/ numlines 5))
                        )))
             (dolist (mento vment)
               (unless (eq funcname mento)
                 (insert
                  (format "  \"%s\" -- \"%s\"\n"
                          oname
                          (package-map--newname mento)))))))
         hashtable)
        (insert "}\n")
        (save-buffer)))))


;; digraph G {

;; 	subgraph cluster_0 {
;; 		style=filled;
;; 		color=lightgrey;
;; 		node [style=filled,color=white];
;; 		a0 -> a1 -> a2 -> a3;
;; 		label = "process #1";
;; 	}

;; 	subgraph cluster_1 {
;; 		node [style=filled];
;; 		b0 -> b1 -> b2 -> b3;
;; 		label = "process #2";
;; 		color=blue
;; 	}


(defun package-map--makedigraphgroups (hashtable colormap shapemap)
  "Make digraph subgraphs for each file cluster, using HASHTABLE files, and sytled using COLORMAP and SHAPEMAP."
  (dolist (vfile (package-map--filesuniq hashtable))
    (insert (format "  subgraph \"%s\" {\n" vfile))
    (insert (format "      node [color=%s];\n" (alist-get vfile colormap)))
    (insert (format "      label = \"%s\";\n" vfile))
    (insert (format "      color=black;\n"))
    ;; First pass define nodes
    (maphash
     (lambda (funcname info)
       ;; Only process functions from VFILE
       (if (eq (plist-get info :file) vfile)
           (let ((oname (package-map--newname funcname))
                 (vbegs (plist-get info :line-beg))
                 (vends (plist-get info :line-end))
                 (vtype (plist-get info :type))
                 (vment (plist-get info :mentions)))
             (let ((numlines (if vends (- vends vbegs) 1)))
               (insert
                (format "      node [shape=%s,penwidth=%s] \"%s\";\n"
                        (alist-get (intern vtype) shapemap)
                        (1+ (/ numlines 5))
                        oname))))))
     hashtable)
    ;; Second pass define intrafile links
    (maphash
     (lambda (funcname info)
       ;; Only process functions from VFILE
       (let ((oname (package-map--newname funcname))
             (vbegs (plist-get info :line-beg))
             (vends (plist-get info :line-end))
             (vtype (plist-get info :type))
             (vment (plist-get info :mentions)))
         (if (eq (plist-get info :file) vfile)
             (dolist (mento vment)
               (unless (eq funcname mento)
                 (let* ((mento-info (gethash mento hashtable))
                        (mento-file (plist-get mento-info :file)))
                   ;; If functions are from the same file,
                   ;; list them here.
                   (if (string= vfile mento-file)
                       (insert
                        (format "      \"%s\" -> \"%s\"\n"
                                oname
                                (package-map--newname mento))))))))))
     hashtable)
    (insert "  }\n")))

(defun package-map--makedigraphcrossinglinks (hashtable colormap shapemap)
  "Make the digraph connections across clusters, using functions from HASHTABLE, and styled using COLORMAP and SHAPEMAP."
  (dolist (vfile (package-map--filesuniq hashtable))
    (maphash
     (lambda (funcname info)
       (let ((oname (package-map--newname funcname))
             (vfile (plist-get info :file))
             (vbegs (plist-get info :line-beg))
             (vends (plist-get info :line-end))
             (vtype (plist-get info :type))
             (vment (plist-get info :mentions)))
         (dolist (mento vment)
           (unless (eq funcname mento)
             (let* ((mento-info (gethash mento hashtable))
                    (mento-file (plist-get mento-info :file)))
               ;; If functions are NOT from the same file,
               ;; list them here.
               (if (not (string= vfile mento-file))
                   (insert
                    (format "  \"%s\" -> \"%s\"\n"
                            oname
                            (package-map--newname mento)))))))))
     hashtable)))

(defun package-map-makedigraphdotfile ()
  "Make a dot file representation of all the top level definitions in a project, and their references."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    ;; TODO: implement these
    (let ((colormap (package-map--makecolormap hashtable))
          (shapemap package-map-parse-function-shapes))
      (with-current-buffer (find-file-noselect "graphviz2.dot")
        (erase-buffer)
        (insert "digraph G {\n")
        (package-map--makedigraphgroups hashtable
                                        colormap
                                        shapemap)
        (package-map--makedigraphcrossinglinks hashtable
                                               colormap
                                               shapemap)
        (insert "}\n")
        (save-buffer)))))



;; https://graphviz.org/doc/info/attrs.html

;; TODO:
;;  * Get borders around Clusters
;;  * Implement arrows between clusters to show how
;;    the 'requires and 'provide work

(provide 'package-map)
;;; package-map.el ends here

;; ;; Testing
;;(setq temphash (package-map-parse--generatemap))
;; (setq linelist (package-map-secondhelp--makesortedlinelist temphash))
;; (--filter (string= (nth 2 it)
;;                    "package-map-parse--generatemap")
;;           linelist)
;; (package-map-secondhelp--updatementionslist
;;  "package-map-parse--generatemap"
;;  (gethash "package-map-parse--generatemap" temphash)
;;  linelist)
