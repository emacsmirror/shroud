;;; shroud-el.el --- Elisp implementation of Shroud

;;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: https://github.com/o-nly/emacs-shroud
;;; Package-Version: 1.15
;;; Keywords: tools, password management
;;; Package-Requires: ((epg "1.0.0") (emacs "25") (s "1.6.0") (dash "2.18.0"))

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

;;; parts required
;;; shroud interpreter: for the shroud machine. A feel:
;;; (shroud-eval bar env-db) -> evaluates to entry "bar", in database env-db
;;; (shroud-eval (define foo <entry>) env-db) -> unspecified, bind foo= <entry>
;;; (shroud-eval (U foo bar) -> entry, (union)foo and bar's contents under foo, no repetition
;;; (shroud-eval (^ foo bar) -> entry, (intersection) foo and bar's same content under foo, no repetition
;;; (shroud-eval (- foo bar) -> entry, foo's contents that are not present in bar
;;; (shroud-eval (+ foo bar) -> entry, foo's contents plus bar's
;;; tests:
;;; (shroud-eval (- (+ foo bar) bar))
;;; desired: variadic (U foo bar ...), composable (+ (U foo bar) baz)

;;; Code:

(require 'epg)
(require 'cl-macs)
(require 's)
(require 'dash)

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
  (let ((fn (lambda (pair)
              (eval `(setq ,(intern (format "shroud-el--%s" (car pair))) ,(cdr pair))))))
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

(defun shroud-el--entry-get (key shroud-entry &optional items)
  "Get KEY from SHROUD-ENTRY.
Optional argument ITEMS ."
  (pcase-let ((`((id . ,id) (contents . ,contents)) shroud-entry))
    (pcase key
      (`all shroud-entry)
      (`id id)
      (`name id)
      (`contents contents)
      (`entry `((id . ,id) (name . ,id) ,@contents))
      ('items (-map (-cut assoc <> contents) items))
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

(defun shroud-el--run-internal (db-file key &optional fn)
  "Run shroud on DB-FILE with KEY FN.
Shroud entry function."
  (let ((db (lambda () (read (shroud-el--file-contents db-file)))))
    (pcase key
      (`db (funcall db))
      (`fn (funcall fn (funcall db))))))

(defun shroud-el--run (db-file &rest args)
  "Run shroud on DB-FILE with ARGS.
Shroud entry function."
  (cl-labels ((db () (shroud-el--run-internal db-file 'db))
            (entry-name (e) (funcall (-cut shroud-el--entry-get 'id <>) e))
            (entry-contents (e) (funcall (-cut shroud-el--entry-get 'entry <>) e))
            (fn-by-id (fn id db mod)
                      (funcall fn (lambda (e)
                                    (funcall mod (equal (or (car-safe id) id) (shroud-el--entry-get 'id e))))
                                              db))
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
       (fn-by-id #'-find a (db) #'identity))
      (`("show" ,a . ,rest)
       (let ((entry (fn-by-id #'-find a (db) #'identity)))
         (-filter (apply #'-orfn (-map #'query-car rest)) (entry-contents entry))))
      (`("hide" ,a)
       (if (and (shroud-el--entry? a)
                (not (shroud-el--entry-exists? a (db))))
           (shroud-el--write-file (lambda () (cons a (db))) db-file)))
      (`("hide" "--edit" ,a)
       (if (shroud-el--entry? a)
           (shroud-el--write-file (lambda ()
                                    (cons a (fn-by-id #'-filter (entry-name a) (db) #'not)))
                                  db-file)))
      (`("remove" ,a)
       (shroud-el--write-file (lambda ()
                                (fn-by-id #'-filter a (db) #'not))
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
        (split (lambda (s) (s-split-up-to " " s 1)))
        (res (cons entry-name
                   (-map make-pair
                         (-map split
                               (-map #'s-collapse-whitespace
                                     (s-split "\n" output-string)))))))
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

(defun shroud-el--input-string->shroud-entry (entry-input-strings)
    "Parse ENTRY-INPUT-STRINGS into a Shroud entry."
    (let ((s entry-input-strings)
          (split-cons (lambda (str) (let ((x (s-split-up-to "=" str 1)))
                                       (cons (car x) (cadr x))))))
      `((id . ,(car s))
        (contents ,@(-map split-cons (cdr s))))))

(defun shroud-el-run (db-file &rest args)
  "Run shroud on DB-FILE with ARGS.
Shroud user entry function."
  (let ((run (-partial #'shroud-el--run db-file)))
    (pcase args
      (`(,(or "--help" "-h") . ,_) "Usage: shroud COMMAND ARGS. COMMAND may be one of:\nlist | show | remove | hide")
      (`("--version" . ,_) "shroud-el 1.12\nCopyright (C) 2019 Amar Singh\nGPLv3 or later")
      (`("list" ,(or "--help" "-h") . ,_) "Usage: shroud list [OPTION]\nShow the names of all secrets in the database.")
      (`("show" ,(or "--help" "-h") . ,_) "Usage: shroud show [OPTION] ID [KEY ...]\nShow secret named ID.")
      (`("hide" ,(or "--help" "-h") . ,_) "Usage: shroud hide [OPTION] ID KEY=VALUE ...\nAdd a new secret named ID to the database.")
      (`("remove" ,(or "--help" "-h") . ,_) "Usage: shroud remove [OPTION] id\nRemove a secret from the database.")
      (`("hide" "--edit" . ,e) (apply run "hide" "--edit" (shroud-el--input-string->shroud-entry e) '()))
      (`("hide" .  ,e) (apply run "hide" (shroud-el--input-string->shroud-entry e) '()))
      (`("show" "--clipboard" ,e . ,fields) (kill-new (s-join "\n" (-map #'cdr (shroud-el--entry-get 'items (apply run  "show" e '()) fields)))))
      (_  (let ((res (apply (-partial #'shroud-el--run db-file) args)))
            (cond
             ((shroud-el--entry? (car res)) (s-join "\n" (-map (-cut shroud-el--entry-get 'id <>) res)))
             ((shroud-el--entry? res) (shroud-el--entry->output-string res))
             (res (s-join "\n" (-map #'cdr res)))))))))

(provide 'shroud-el)

;;; shroud-el.el ends here
