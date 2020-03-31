;;; package-map.el --- Generate a graphviz map of functions and definitions -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/remind-bindings.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1"))
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

;; This package generates a graphviz DOT map of all the functions and variables
;; in a given elisp package.  The aim is to help developers see how tangled their
;; code is and to help them refactor it.
;;
;; More info at https://graphviz.org/doc/info/attrs.html

;;; Code:
(require 'package-map-graph)
(require 'package-map-exec)

(require 'org-table)
(require 'subr-x)

;;;###autoload
(defun package-map-makesummarytable ()
  "Make a summary org table of variables and references to them."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    (with-current-buffer
        (find-file (format "%s.%s"
                           (car (split-string package-map-exec-file "\\."))
                           "org"))
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
           (insert (format "| %s | %d | %s | %s | %d | %s |\n"
                           vtype
                           (if vends (- vends vbegs) 1)
                           funcname
                           vfile
                           (length vment)
                           vment))))
       hashtable)
      (org-table-align)
      (save-buffer))))


;;;###autoload
(defun package-map-graphviz-digraph (&optional shuffle)
  "Make a dot file representation of all definitions and references.
Optionally set INDENT-WIDTH which is 2 by default.
If SHUFFLE gives a random seed (default 0) to shuffle subgraph cluster layouts."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap shuffle))
        (fn-decorate #'package-map-graph--decorate)
        (fn-digraph #'package-map-graph--makedigraphgroups)
        (fn-dicross #'package-map-graph--makedigraphcrossinglinks)
        (fn-execshow #'package-map-exec--executeandshow)
        (vr-funcmap package-map-parse-function-shapes)
        (vr-indwidth package-map-graph-indentwidth))
    (let ((filemap (package-map-graph--makefilemapcolors hashtable))
          (ind-now (make-string vr-indwidth ? ))
          (decor-graph (funcall fn-decorate :graph))
          (decor-node (funcall fn-decorate :node))
          (decor-edge (funcall fn-decorate :edge)))
      (with-current-buffer (find-file-noselect package-map-exec-file)
        (erase-buffer)
        (insert "digraph G {\n")
        (insert (format "%sgraph %s;\n" ind-now decor-graph))
        (if decor-node (insert (format "%snode %s;\n" ind-now decor-node)))
        (if decor-edge (insert (format "%sedge %s;\n" ind-now decor-edge)))
        ;; --{external inserts}--
        (funcall fn-digraph hashtable filemap vr-funcmap vr-indwidth)
        (funcall fn-dicross hashtable filemap vr-indwidth)
        ;; --
        (insert "}\n")
        (save-buffer)
        (funcall fn-execshow)))))


;;;###autoload
(defun package-map-graphviz ()
  "Make a very basic dot file representation of all the top level definitions in a project, and their references."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    (let ((filemap (package-map-graph--makefilemapcolors hashtable))
          (funcmap package-map-parse-function-shapes))
      (with-current-buffer (find-file-noselect package-map-exec-file)
        (erase-buffer)
        (insert "strict graph {\n")
        (maphash
         (lambda (funcname info)
           (let ((vfile (plist-get info :file))
                 (vbegs (plist-get info :line-beg))
                 (vends (plist-get info :line-end))
                 (vtype (plist-get info :type))
                 (vment (plist-get info :mentions)))
             (let* ((numlines (if (and vends (not (eq vtype 'defun))) (- vends vbegs) 1))
                    (fileentry (--first (string= (plist-get it :file) vfile) filemap))
                    (filecolor (plist-get fileentry :color))
                    (filesymbl (plist-get fileentry :symbol))
                    (funcshape (alist-get (intern vtype) funcmap))
                    (linemods (1+ (/ numlines package-map-graph-linemod)))
                    (oname (package-map-graph--newname funcname vfile filesymbl)))
               (insert (format "  \"%s\" [shape=%s,color=%s,penwidth=%s]\n"
                               oname
                               funcshape
                               filecolor
                               linemods))
               (dolist (mento vment)
                 (unless (eq funcname mento)
                   (let* ((mento-entry (gethash mento hashtable))
                          (mento-file (plist-get mento-entry :file))
                          (mento-fileinfo (--first (string= (plist-get it :file)
                                                            mento-file)
                                                   filemap))
                          (mento-symb (plist-get mento-fileinfo :symbol)))
                     (insert (format "  \"%s\" -- \"%s\";\n"
                                     oname
                                     (package-map-graph--newname mento
                                                                 mento-file
                                                                 mento-symb)))))))))
         hashtable)
        (insert "}\n")
        (save-buffer)
        (package-map-exec--executeandshow)))))


;; TODO:
;;  * Implement arrows between clusters to show how
;;    the 'requires and 'provide work

(provide 'package-map)
;;; package-map.el ends here

