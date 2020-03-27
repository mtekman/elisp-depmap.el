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
(require 'package-map-graph)
(require 'package-map-exec)

(require 'org-table)
(require 'subr-x)

(defun package-map-makesummarytable ()
  "Make a summary org table of variables and references to them."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    (with-current-buffer (find-file-other-frame "graphviz2.org")
      (erase-buffer)
      (insert "| Type | #Lines | Name | File | #Mentions | Mentions |\n|--\n")
      (maphash
       (lambda (funcname info)
         (let ((vfile (plist-get info :file))
               (vbegs (plist-get info :line-beg))
               (vends (plist-get info :line-end))
               (vtype (plist-get info :type))
               (vment (--filter (not (string= funcname it))
                                (plist-get info :mentions))))
           (insert
            (format "| %s | %d | %s | %s | %d | %s |\n"
                    vtype
                    (if vends (- vends vbegs) 1)
                    funcname
                    vfile
                    (length vment)
                    vment))))
       hashtable)
      (org-table-align))))


(defun package-map-graphviz-digraph (&optional surround)
  "Make a dot file representation of all the top level definitions in a project, and their references.  If SURROUND, then group the functions of each file."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    ;; TODO: implement these
    (let ((colormap (package-map-graph--makefilemapcolors hashtable))
          (shapemap package-map-parse-function-shapes))
      (with-current-buffer (find-file-noselect package-map-exec-file)
        (erase-buffer)
        (insert "digraph G {\n")
        (package-map-graph--makedigraphgroups hashtable colormap shapemap surround)
        (package-map-graph--makedigraphcrossinglinks hashtable)
        (insert "}\n")
        (save-buffer)
        (package-map-exec--executeandshow)))))


(defun package-map-graphviz ()
  "Make a dot file representation of all the top level definitions in a project, and their references."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    (let ((colormap (package-map-graph--makefilemapcolors hashtable))
          (shapemap package-map-parse-function-shapes))
      (with-current-buffer (find-file-noselect package-map-exec-file)
        (erase-buffer)
        (insert "strict graph {\n")
        (maphash
         (lambda (funcname info)
           (let ((oname (package-map-graph--newname funcname))
                 (vfile (plist-get info :file))
                 (vbegs (plist-get info :line-beg))
                 (vends (plist-get info :line-end))
                 (vtype (plist-get info :type))
                 (vment (plist-get info :mentions)))
             (let ((numlines (if vends (- vends vbegs) 1)))
               (insert (format "  \"%s\" [shape=%s,color=%s,penwidth=%s]\n"
                               oname
                               (alist-get (intern vtype) shapemap)
                               (alist-get vfile colormap)
                               (1+ (/ numlines 5)))))
             (dolist (mento vment)
               (unless (eq funcname mento)
                 (insert (format "  \"%s\" -- \"%s\"\n"
                                 oname
                                 (package-map-graph--newname mento)))))))
         hashtable)
        (insert "}\n")
        (save-buffer)
        (package-map-exec--executeandshow)))))



;; https://graphviz.org/doc/info/attrs.html

;; TODO:
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
