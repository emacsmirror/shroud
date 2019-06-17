;;; shroud-el.el --- Elisp implementation of Shroud

;;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: https://github.com/o-nly/emacs-shroud
;;; Package-Version: 1.15
;;; Keywords: tools, password management
;;; Package-Requires: ((epg "1.0.0") (emacs "25") (s "1.6.0") (dash "2.12.0") (dash-functional "2.12.0"))

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; An alternative to shroud command line.
;;; what shall i keep in this module? Since this is a shroud
;;; implementation I would need to support all of shroud's command line
;;; options, but I dont feel like writing crappy code based on
;;; optargs, We can write convenient facilities in Lisp, I'll have a
;;; wrapper around it (a serializer so that external modules can choose
;;; to use it as if this were shroud itself.)

;;; parts required
;;; 1. shroud--db-reader DONE
;;; 2. shroud--db-writer DONE
;;; 3. shroud--run- DONE
;;; 3.a help DONE
;;; 3.b list DONE
;;; 3.c show DONE
;;; 3.d hide DONE
;;; 3.e remove DONE
;;; 4. shroud--entry type/object DONE
;;; 5. shroud--entry-name DONE

;;; TODO
;;; 0. `require' DONE
;;; 1. detect missing `shroud-database-file' DONE
;;; 2. detect missing `shroud-config-file' DONE
;;; 3. `tests' (assert (equal (shroud--run-el args) (shroud--run-internal args))) DONE
;;; 4. `shroud-gpg-key', (or alternative-method) ABORTED
;;; 5. functional style DONE
;;; 6. Deprecate features (add an alternative in "Docstring `alternative'") DONE

;;; Code:

(require 'epg)
(require 'cl-macs)
(require 's)
(require 'dash)
(require 'dash-functional)

(defun shroud-el--file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defcustom shroud-el--database-file (concat (getenv "HOME") "/.config/shroud/db.gpg")
  "Shroud Datastore file.
GPG Encrypted."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--user-id nil
    "Shroud User ID."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--config-file (concat (getenv "HOME") "/.shroud")
  "Shroud Config file."
  :group 'shroud
  :type 'file)

(defun shroud-el--apply-config! (filename)
  "Apply configurations from shroud configuration in FILENAME."
  (let ((fn (lambda (pair) (eval `(setq ,(intern (format "shroud-el--%s" (car pair))) ,(cdr pair))))))
    (-map fn (eval (read (shroud-el--file-contents filename))))))
(if (and shroud-el--config-file (file-exists-p shroud-el--config-file))
    (shroud-el--apply-config! shroud-el--config-file))

(defun shroud-el--write-file (reader filename)
  "Write the output of READER to FILENAME."
  (with-temp-buffer
    (when (equal "gpg" (file-name-extension filename))
        (when shroud-el--user-id
            (insert (format ";; -*- epa-file-encrypt-to: (\"%s\") -*-\n" shroud-el--user-id))))
    (insert (format "%S" (funcall reader)))
    (write-file filename)))

(defun shroud-el--query (q)
   "Build a query Procedure for querying Q."
  (-partial #'s-matches? q))

(defun shroud-el--entry-get (key shroud-entry)
  "Get KEY from SHROUD-ENTRY."
  (pcase-let ((`((id . ,id) (contents . ,contents)) shroud-entry))
    (pcase key
      (`all shroud-entry)
      (`id id)
      (`name id)
      (`contents contents)
      (`entry `((id . ,id) (name . ,id) ,@contents))
      (_ (assoc key contents)))))

(defun shroud-el--entry? (entry)
  "Check if the ENTRY is a valid shroud-el--entry."
  (pcase entry
    (`((id . ,id) (contents . ,contents)) t)))

(defun shroud-el--entry-exists? (entry db)
  "Check if the ENTRY is a valid shroud-el--entry not already present in DB."
  (and (-find (shroud-el--query (shroud-el--entry-get 'id entry))
              (-map (-cut shroud-el--entry-get 'id <>) db))
       t))

(defun shroud-el--run (db-file &rest args)
  "Run shroud on DB-FILE with ARGS.
Shroud entry function."
  (cl-labels ((db () (read (shroud-el--file-contents db-file)))
            (entry-name (e) (funcall (-cut shroud-el--entry-get 'id <>) e))
            (entry-contents (e) (funcall (-cut shroud-el--entry-get 'entry <>) e))
            (check  (a b) (equal a (car b)))
            (query-car (q) (-partial #'check q))
            (query-name (x y) (s-matches? x (entry-name y)))
            (make-query (q) (-partial #'query-name q)))
    (pcase args
      (`(db ,fn) (funcall fn (db)))
      (`(entry ,entry ,fn) (funcall fn (-find #'(lambda (e) (equal (entry-name e) entry)) (db))))
      (`("list") (db))
      (`("list" . ,a) (-filter (apply #'-orfn
                                      (-map #'make-query a))
                               (db)))
      (`("show" ,a)
       (-find #'(lambda (e) (equal (entry-name e) a)) (db)))
      (`("show" ,a . ,rest)
       (let ((entry (-find #'(lambda (e) (equal (entry-name e) a)) (db))))
         (-filter (apply #'-orfn (-map #'query-car rest)) (entry-contents entry))))
      (`("hide" ,a)
       (if (and (shroud-el--entry? a)
                (not (shroud-el--entry-exists? a (db))))
           (shroud-el--write-file (lambda () (cons a (db))) db-file)))
      (`("hide" "--edit" ,a)
       (if (and (shroud-el--entry? a)
                (shroud-el--entry-exists? a (db)))
           (shroud-el--write-file
            (lambda ()
              (cons a
                    (-filter #'(lambda (c)
                                 (not (equal (entry-name c) (entry-name a))))
                             (db))))
            db-file)))
      (`("remove" ,a)
       (shroud-el--write-file (lambda ()
                                (-filter #'(lambda (c)
                                             (not (equal (entry-name c) a)))
                                         (db)))
                              db-file)))))

(defun shroud-el--entry->output-string (e)
  "ENTRY E."
  (s-join "\n"
          (-map #'(lambda (pair)
                    (concat (car pair) " " (cdr pair)))
                (shroud-el--entry-get 'contents e))))

(defun shroud-el--output-string->input-string (entry-name output-string &optional split?)
  "ENTRY-NAME OUTPUT-STRING.
If OPTIONAL SPLIT? is provided then split the outputs."
  (let* ((make-pair (lambda (ls) (concat (car ls) "=" (cadr ls))))
        (split (lambda (s) (split-string-and-unquote s)))
        (res (cons entry-name
                   (-map make-pair
                         (-map split
                               (s-split "\n" output-string))))))
    (if split? res
      (s-join " " res))))

(defun shroud-el--entry->input-string (e &optional split?)
  "Parse entry E into a Shroud CLI compatible string.
If OPTIONAL SPLIT? is provided then split the outputs."
  (let ((res (cons (shroud-el--entry-get 'name e)
                (-map #'(lambda (pair)
                          (concat (car pair) "=" (cdr pair)))
                      (shroud-el--entry-get 'contents e)))))
    (if split? res
      (s-join " " res))))

(defun shroud-el--input-string->shroud-entry (entry-input-string &optional split? seperator)
    "Parse ENTRY-INPUT-STRING into a Shroud entry.
If optional SPLIT? is provided then split the strings before
conversion and use SEPERATOR, if present, to split."
    (let ((s (if (not split?) entry-input-string
               (split-string-and-unquote entry-input-string (or seperator " ")))))
    (cl-labels ((split-cons (str) (let ((s (s-split "=" str)))
                                      (cons (car s) (cadr s)))))
      `((id . ,(car s))
        (contents ,@(-map #'split-cons (cdr s)))))))

(defun shroud-el-run (db-file &rest args)
  "Run shroud on DB-FILE with ARGS.
Shroud user entry function."
  (pcase args
    (`(,(or "--help" "-h") . ,_) "Usage: shroud COMMAND ARGS. COMMAND may be one of:\nlist | show | remove | hide")
    (`("--version" . ,_) "shroud-el 1.12\nCopyright (C) 2019 Amar Singh\nGPLv3 or later")
    (`("list" ,(or "--help" "-h") . ,_) "Usage: shroud list [OPTION]\nShow the names of all secrets in the database.")
    (`("show" ,(or "--help" "-h") . ,_) "Usage: shroud show [OPTION] ID [KEY ...]\nShow secret named ID.")
    (`("hide" ,(or "--help" "-h") . ,_) "Usage: shroud hide [OPTION] ID KEY=VALUE ...\nAdd a new secret named ID to the database.")
    (`("remove" ,(or "--help" "-h") . ,_) "Usage: shroud remove [OPTION] id\nRemove a secret from the database.")
    (`("hide" "--edit" . ,e) (apply (-partial #'shroud-el--run db-file) "hide" "--edit" (shroud-el--input-string->shroud-entry e) '()))
    (`("hide" .  ,e) (apply (-partial #'shroud-el--run db-file) "hide" (shroud-el--input-string->shroud-entry e) '()))
    (`("show" "--clipboard" ,e . ,fields) (kill-new (s-join "\n" (-map #'cdr (apply (-partial #'shroud-el--run db-file) "show" e fields)))))
    (_  (let ((res (apply (-partial #'shroud-el--run db-file) args)))
          (cond
           ((shroud-el--entry? (car res)) (s-join "\n" (-map (-cut shroud-el--entry-get 'id <>) res)))
           ((shroud-el--entry? res) (shroud-el--entry->output-string res))
           (res (s-join "\n" (-map #'cdr res))))))))

(provide 'shroud-el)

;;; shroud-el.el ends here
