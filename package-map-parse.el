;; Go through all files using projectile and retrieve top level definitions and their positions
;; then determine which functions are called by which others.


(defun getsourcefiles ()
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-project-files project-root))
         (project-srcs (--filter (string-suffix-p ".el" it) project-files)))
    project-srcs))
 
(defun nexttopdef ()
  "Find all definitions. If functions, get their bounds"
  "(\\(setq\\|\\(def\\(un\\|var\\|subst\\)\\)\\)\ "
  (search-forward "(use-package")
  (beginning-of-line)
  (let* ((bound (funcall show-paren-data-function))
         (inner (nth 0 bound))
         (outer (nth 3 bound)))
    (if (not bound)
        (progn (move-end-of-line 1) nil)
      (search-forward "use-package " outer t)
      (let* ((beg (point))
             (end (progn
                    (search-forward-regexp "\\( \\|)\\|$\\)" outer)
                    (point)))
             (name (string-trim (buffer-substring-no-properties beg end))))
        (goto-char outer)
        `(,name ,inner ,outer)))))

