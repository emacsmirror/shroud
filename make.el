;;; make --- Build project

;;; Commentary:
;;; tiny script to build project from source files, and source documentation.

;;; Code:
(require 'bytecomp)

(defvar make-inputs '("shroud-el.el"  "shroud-cli.el" "shroud-bui.el" "shroud.el"))
(defvar output-dir "bin/emacs/")

(defun make-output-dir (dir)
  "Make DIR if not already exists."
    (if (file-directory-p dir) 0
      (make-directory dir t)))

(defun copy-files (files dir)
  "Copy FILES to DIR."
  (mapc #'(lambda (file)
          (copy-file file dir t nil nil t))
      files))

(defun incremental-compile (files)
  "Compile and load FILES in order."
  (let ((file (car files)))
    (byte-compile-file file)
    (load-file (concat file "c"))
    (incremental-compile (cdr files))))

(defun with-directory-excursion (dir thunk)
  "Switch to DIR before performing THUNK.
Return to the previous state on exit."
  (let ((cur default-directory))
    (cd dir)
    (eval thunk)
    (cd cur)))

(make-output-dir output-dir)

(copy-files make-inputs output-dir)

(with-directory-excursion output-dir
 '(incremental-compile make-inputs))

0
;;; make.el ends here
