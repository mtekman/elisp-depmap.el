;; Go through all files using projectile and retrieve top level definitions and their positions
;; then determine which functions are called by which others.

(require 'paren)

(defun getsourcefiles ()
  "Find all source files from the current project."
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-project-files project-root)))
    (--filter (string-suffix-p ".el" it) project-files)))

(defun alltopdefs-file (file hashdefs)
  "Get all top definitions in FILE and put into HASHDEFS.
Don't use grep or projectile, because those sonuvabitch finish hooks are not reliable."
  (with-current-buffer (find-file-noselect file)
    (goto-char 0)
    (let ((reg-type "^(\\(setq\\|\\(def\\(un\\|var\\|subst\\|custom\\)\\)\\)")
          (reg-vnam "\\(-?\\w+\\)+"))
      (while (search-forward-regexp reg-type nil t)
        ;; Get type
        (let* ((type-end (point))
               (type-beg (1+ (move-beginning-of-line 1)))
               (type-nam (buffer-substring-no-properties type-beg type-end)))
          (goto-char type-end)
          (forward-whitespace 1)
          ;; Get variable name
          (let* ((vnam-beg (point))
                 (vnam-end (search-forward-regexp reg-vnam))
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
                       `(:type ,type-nam :line-beg ,lnum-beg :line-end ,lnum-end :mentions nil)
                       hashdefs)))))
      hashdefs)))

(defun alltopdefs-filelist (filelist)
  "Get all top definitions from FILELIST and return a hashtable, with
variable names as keys as well as type and bounds as values."
  (let ((hashtable (make-hash-table :size 1000)))
    (dolist (pfile filelist hashtable)
      (alltopdefs-file pfile hashtable))))

(defun allsecondarydefs-file (file hashtable)
  "Get all secondary definitions in FILE for the
top-level definitions in HASHTABLE."
  (let ((linelist nil))
    (maphash
     (lambda (nam vals)
       (let ((lbeg (plist-get vals :line-beg))
             (lend (plist-get vals :line-end)))
         ;; -- functions only
         (if lend
             (cl-pushnew `(,lbeg ,lend ,nam) linelist))))
     hashtable)
    ;; -- Check each top def in the buffer
    (with-current-buffer (find-file-noselect file)
      (maphash  ;; iterate hashtable
       (lambda (vname type-bounds-info)
         (goto-char 0)
         (let ((vnam-regex (format "\\( \\|(\\)%s\\( \\|)\\)" vname))
               (mentionlist nil))
           (while (search-forward-regexp vnam-regex nil t)
             (let ((lnum (line-number-at-pos)))
               ;; Find the calling function by checking the line list
               (dolist (lelm linelist)
                 (let ((name (nth 2 lelm))
                       (lbeg (nth 0 lelm))
                       (lend (nth 1 lelm)))
                   (if (<= lbeg lnum lend)
                       ;; update entry to add
                       (let* ((hashval type-bounds-info))
                              (mention (plist-get hashval :mentions)))
                         (cl-pushnew name mention)
                         (setf hashval mention)))))))))))) ;; -- needed, or implicit?



(defun allsecondarydefs-filelist (filelist hashtable)
  "Get all secondary definitions for all files in FILELIST
for the top-level definitions in HASHTABLE."
  (dolist (pfile filelist hashtable)
    (allsecondarydefs-file pfile hashtable)))

(defun populate-project ()
  "Project."
  (let ((proj-files (getsourcefiles))
        (hash-table (alltopdefs-filelist proj-files)))
    (allsecondarydefs-filelist proj-files hash-table)))
