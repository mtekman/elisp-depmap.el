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


(defcustom package-map-dot-file "~/graphviz2.dot" ;
  "Location of dot file. The output image file will use the prefix before the extension."
  :type 'string
  :group 'package-map)

(defcustom package-map-dot-outext "png"
  "Output file type"
  :type 'string
  :options '("png" "svg" "tiff" "jpeg" "eps" "json")
  :group 'package-map)

(defcustom package-map-dot-commandargs ""
  "Other command line args"
  :type 'string
  :group 'package-map)

(defun package-map-dot--executeandshow ()
  "Execute the dotfile command and then show the graph."
  (let* ((outfile (format "%s.%s"
                         (car (split-string package-map-dot-file "\\."))
                         package-map-dot-outext))
         (outputf (shell-command-to-string
                   (format "dot %s -T%s %s -o %s"
                           package-map-dot-file
                           package-map-dot-outext
                           package-map-dot-commandargs
                           outfile))))
    (find-file-other-frame outfile)))


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


(defun package-map--makedigraphgroups (hashtable colormap shapemap
                                                 &optional surround)
  "Make digraph subgraphs for each file cluster, using HASHTABLE files, and styled using COLORMAP and SHAPEMAP. 
If SURROUND, box the functions in each file."
  (let* ((filelist (package-map--filesuniq hashtable))
         (clustnames (mapcar* #'cons
                              filelist
                              (--map (format "cluster_%d" it)
                                     (number-sequence 0 (1- (length filelist))))))
         (subnames (if surround clustnames)))
    (dolist (vfile filelist)
      (insert (format "  subgraph %s {\n" (if subnames
                                                  (alist-get vfile clustnames)
                                            (format "\"%s\"" vfile))))
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
                          (format "      \"%s\" -> \"%s\";\n"
                                  oname
                                  (package-map--newname mento))))))))))
       hashtable)
      (insert "  }\n"))))

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


(defun package-map-makedigraphdotfile (&optional surround)
  "Make a dot file representation of all the top level definitions in a project, and their references. If SURROUND, then group the functions of each file."
  (interactive)
  (let ((hashtable (package-map-parse--generatemap)))
    ;; TODO: implement these
    (let ((colormap (package-map--makecolormap hashtable))
          (shapemap package-map-parse-function-shapes))
      (with-current-buffer (find-file-noselect package-map-dot-file)
        (erase-buffer)
        (insert "digraph G {\n")
        (package-map--makedigraphgroups hashtable
                                        colormap
                                        shapemap
                                        surround)
        (package-map--makedigraphcrossinglinks hashtable
                                               colormap
                                               shapemap)
        (insert "}\n")
        (save-buffer)
        (package-map-dot--executeandshow)))))

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
