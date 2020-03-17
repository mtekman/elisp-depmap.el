;; Go through all files using projectile and retrieve top level definitions and their positions
;; then determine which functions are called by which others.

(require 'paren)

(defun getsourcefiles ()
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-project-files project-root)))
    (--filter (string-suffix-p ".el" it) project-files)))

(defun alltopdefs-file (file hashdefs)
  "Get all top definitions in FILE.  Don't use grep, because the finished hook is not reliable."
  (let ((reg-type "^(\\(setq\\|\\(def\\(un\\|var\\|subst\\|custom\\)\\)\\)")
        (reg-vnam "\\(-?\\w+\\)+"))
    (while (search-forward-regexp reg-type nil t)
      ;; Get type
      (let* ((type-end (point))
             (type-beg (1+ (move-beginning-of-line 1)))
             (type-name (buffer-substring-no-properties type-beg type-end)))
        (goto-char type-end)
        (forward-whitespace 1)
        ;; Get variable name
        (let* ((vnam-beg (point))
               (vnam-end (search-forward-regexp reg-vnam))
               (vnam-name (buffer-substring-no-properties vnam-beg vnam-end)))
          ;; Get bounds or line number
          (let ((lnum-beg (line-number-at-pos))
                (lnum-end nil))
            (when (string= type-name "defun")
              (move-beginning-of-line 1)
              (let* ((bounk (funcall show-paren-data-function))
                     (keybf (nth 0 bounk))
                     (keybl (nth 3 bounk)))
                (goto-char keybl)
                (setq lnum-end (line-number-at-pos))))
            (puthash vnam-name `(,type-name ,lnum-beg ,lnum-end nil) hashdefs)))))
    hashdefs))

(defun alltopdefs-project ()
  "Get all top definitions in project."
  (let ((pfilelist (getsourcefiles))
        (hashtab (make-hash-table :size 1000)))
    (dolist (pfile pfilelist hashtab)
      (alltopdefs-file pfile hashtab))))



;; (defun populatemap ()
;;   "Populate definition map. [topdef, bounds] -> [appearances in other topdefs]"
;;   (let ((file-list (getsourcefiles))
;;         (all-topdf (firstpass file-list))
;;         (topdf-map (secondpass file-list all-topdf)))
;;     topdf-map))
