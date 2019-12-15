;;; make --- Build project

;;; Commentary:
;;; tiny script to build project from source files, and source documentation.

;;; Code:
(defvar make-inputs '("shroud-bui.el"  "shroud-cli.el" "shroud.el" "shroud-el.el"))

(defvar output-dir "bin/emacs/")

;;; generate the compiled files
(mapc #'byte-compile-file make-inputs)

;;; the output of compilation
(defvar make-outputs
  (mapcar #'(lambda (file)
              (concat file "c"))
                        make-inputs))

;;; create an output directory
(if (file-directory-p output-dir) 0
  (make-directory output-dir t))

;;; move ouputs to output directory
(mapc #'(lambda (file)
          (rename-file file output-dir))
      make-outputs)

;;; return 0
0

;;; make.el ends here
