;;; package-map-graph.el --- Generate a graphviz map of functions and definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

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

(defcustom package-map-graph-decoratesubgraph
  '((style . rounded) (bgcolor . white) (fontsize . 25.0) (labelfloat . true) (fontname . "\"times bold\""))
  "Attributes to decorate subgraph with."
  :type 'alist
(defcustom package-map-graph-decorate
  '(:graph
    ((penwidth . 3) (pencolor . black) (bgcolor . grey) (style . rounded) (splines . ortho))
    :subgraph
    ((style . rounded) (bgcolor . transparent) (fontsize . 25.0) (labelfloat . true) (fontname . "\"times bold\""))
    :subsubgraph
    ((style . rounded) (bgcolor . white) (fontsize . 25.0) (labelfloat . true) (fontname . "\"times bold\"")))
  "Attributes to give to the main :graph, the :subgraph (file clusters), and the :subsubgraph (groups defined by `package-map-parse-subclustergroups')."
  :type 'plist
  :group 'package-map)

(defvar package-map-graph--colors-available
  '(red blue darkgreen orange purple gray green yellow pink brown navy maroon violet))

(defvar package-map-graph--symbols-available
  '("ᚻ" "ᛉ" "ᛊ" "ᛋ" "ᛗ" "ᛝ" "ᛢ" "ᛪ" "ᛯ" "ᛸ" "ᛒ" "ᚷ" "ᚫ" "ᚣ" "ŧ" "Ω" "Æ" "þ"))


(defun package-map-graph--decorate (keyword &optional indent)
  "Generate format string for KEYWORD from `package-map-graph-decorate'. If INDENT is nil, all properties are inlined into square brackets, otherwise each property is seperated by a newline followed by the INDENT amount in spaces."
  (let ((func-lay (lambda (x) (format "%s=%s" (car x) (cdr x))))
        (keyw-lst (plist-get package-map-graph-decorate keyword))
        (inds-spc (if indent (make-string indent ? ) "")))
    (if indent
        (concat (mapconcat func-lay keyw-lst
                           (concat ";\n" inds-spc)) ";")
      (format "[%s]" (mapconcat func-lay keyw-lst ";")))))


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

(defun package-map-graph--newname (functionname filename &optional symbol)
  "Strip the projectname from FUNCTIONNAME, or use the FILENAME as the prefix to strip off.  If SYMBOL, use that as replacement."
  (let* ((prool (car (split-string filename "\\.el")))
         (pregx (format "^%s" prool)))
    (if package-map-graph-stripprojectname
        (replace-regexp-in-string pregx (or symbol "§") functionname)
      functionname)))

(defun package-map-graph--decorate-subgraph ()
  "Generate format string for `package-map-graph-decoratesubgraph'."
  (mapconcat (lambda (x) (format "      %s=%s;" (car x) (cdr x)))
             package-map-graph-decoratesubgraph
             "\n"))


(defun package-map-graph--makesubsubgraph (hashtable funcmap entry subg ind)
  "Make a sub subgraph for file ENTRY info using the SUBG keyword from `package-map-parse-subclustergroups' from HASHTABLE.  Use FUNCMAP for shapes, and use IND to set the indent number."
  (let ((vfile (plist-get entry :file))
        (color (plist-get entry :color))
        (clust (plist-get entry :clust))
        (symbl (plist-get entry :symbol))
        (nex-ind (+ ind package-map-graph-indentwidth))
        (vr-subclust package-map-parse-subclustergroups)
        (vr-linemods package-map-graph-linemod)
        (fn-graphdec #'package-map-graph--decorate)
        (fn-newnames #'package-map-graph--newname))
    (let ((accepted-stypes (--map (format "%s" it) (plist-get vr-subclust subg)))
          (clust-keyword (concat
                          clust (string-remove-prefix ":" (format "%s" subg))))
          (ind-now (make-string ind ? ))
          (ind-nex (make-string nex-ind ? )))      
      (insert "\n"
              ind-now (format "subgraph %s {\n" clust-keyword)
              ind-nex (format "label = \"%s\";\n" subg)
              ind-nex (funcall fn-graphdec :subsubgraph nex-ind) "\n")
      (maphash
       (lambda (funcname info)
         ;; Only process functions from VFILE
         (if (string= (plist-get info :file) vfile)
             (let ((oname (funcall fn-newnames funcname vfile symbl))
                   (vbegs (plist-get info :line-beg))
                   (vends (plist-get info :line-end))
                   (vtype (plist-get info :type)))
               (if (member vtype accepted-stypes)
                   (let ((numlines (if vends (- vends vbegs) 1)))
                     (insert ind-nex
                             (format
                              "node [shape=%s,penwidth=%s] \"%s\";\n"
                              (alist-get (intern vtype) funcmap)
                              (1+ (/ vr-linemods)) oname)))))))
       hashtable)
      (insert ind-now "}\n"))))

(defun package-map-graph--makedigraphgroups (hashtable filemap funcmap ind)
  "Make digraph subgraphs for each file cluster, using files from HASHTABLE.
Decorate them using colors from FILEMAP and shapes from FUNCMAP.  Set indent by IND amount."
  (let* ((next-ind (+ ind package-map-graph-indentwidth))
         (ind-prev (make-string ind ? ))
         (ind-next (make-string next-ind ? )))
    (dolist (vfile (--map (plist-get it :file) filemap))
      (let ((entry (--first (string= (plist-get it :file) vfile) filemap))
            (fn-newnames #'package-map-graph--newname)
            (fn-subgraph #'package-map-graph--makesubsubgraph)
            (fn-decorate #'package-map-graph--decorate)
            (vr-striproj package-map-graph-stripprojectname)
            (vr-subclust package-map-parse-subclustergroups))
        (let ((subg-keys  ;; Not how plists are meant to be used...
               (--filter (string-prefix-p ":" (format "%s" it)) vr-subclust))
              (color (plist-get entry :color))
              (symbl (plist-get entry :symbol))
              (clust (plist-get entry :clust)))
          (insert "\n"
                  ind-prev (format "subgraph %s {\n" clust)
                  ind-next (funcall fn-decorate :subgraph next-ind) "\n"
                  ind-next (format "label = \"%s\";\n"
                                   (if vr-striproj
                                       (format "[%s] %s" symbl vfile) vfile))
                  ind-next (format "edge [color=%s];\n" color)
                  ind-next (format "node [color=%s];\n" color))
          ;; First pass define nodes
          ;; - Create subsubgraphs based on keys in `vr-subclust'.
          (dolist (subg subg-keys)
            (funcall fn-subgraph hashtable funcmap entry subg next-ind))
          ;;
          ;; Second pass define intrafile links
          (maphash
           (lambda (funcname info)
             ;; Only process functions from VFILE
             (let ((oname (funcall fn-newnames funcname vfile symbl))
                   (vment (plist-get info :mentions)))
               (if (eq (plist-get info :file) vfile)
                   (dolist (mento vment)
                     (unless (eq funcname mento)
                       (let* ((mento-info (gethash mento hashtable))
                              (mento-file (plist-get mento-info :file)))
                         ;; Only use functions are from the same file
                         (if (string= vfile mento-file)
                             (insert
                              ind-next (format
                                        "\"%s\" -> \"%s\";\n"
                                        oname
                                        (funcall fn-newnames
                                                 mento vfile symbl))))))))))
           hashtable)
          (insert ind-prev "}\n"))))))


(defun package-map-graph--makedigraphcrossinglinks (hashtable filemap ind)
  "Make the digraph connections across clusters, using functions from HASHTABLE, and FILEMAP info. Indent by IND amount."
  (let ((indent (make-string ind ? )))
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
                   (insert indent
                           (format
                            "  \"%s\" -> \"%s\";\n"
                            oname
                            (package-map-graph--newname mento
                                                        mento-file
                                                        mento-symb))))))))))
     hashtable)))


(provide 'package-map-graph)
;;; package-map-graph.el ends here
