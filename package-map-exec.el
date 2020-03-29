;;; package-map-exec.el --- construct the DOT executable -*- lexical-binding: t; -*-

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

;; See package-map.el

;;; Code:
(defcustom package-map-exec-file "~/graphviz2.dot" ;
  "Location of dot file.  The output image file will use the prefix before the extension."
  :type 'string
  :group 'package-map)

(defcustom package-map-exec-outext "png"
  "Output file type."
  :type 'string
  :options '("png" "svg" "tiff" "jpeg" "eps" "json")
  :group 'package-map)

(defcustom package-map-exec-commandargs ""
  "Other command line args for dot executable."
  :type 'string
  :group 'package-map)

(defun package-map-exec--executeandshow ()
  "Execute the dotfile command and then show the graph."
  (let* ((outfile (format "%s.%s"
                          (car (split-string package-map-exec-file "\\."))
                          package-map-exec-outext))
         (command (format "dot %s -T%s %s -o %s"
                          package-map-exec-file
                          package-map-exec-outext
                          package-map-exec-commandargs
                          outfile))
         (dmesg (shell-command-to-string command)))
    (find-file-noselect outfile)
    `(,command . ,dmesg)))

(provide 'package-map-exec)
;;; package-map-exec.el ends here
