;;; shroud-el.el --- Elisp implementation of Shroud

;;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: http://git.nly.info.tm:9001/shroud.git
;;; Package-Version: 1.12
;;; Keywords: tools, password management
;;; Package-Requires: ((epg "1.0.0") (dash "2.15.0") (emacs "25") (f "0.20"))

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

(require 'f)
(require 'epg)
(require 'dash)
(require 'dash-functional)
(require 'cl-macs)

(defun shroud-el--~ (file)
  "Find FILE in user HOME."
    (f-join (getenv "HOME") file))

(defun shroud-el--file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defcustom shroud-el--database-file
  (let ((default (shroud-el--~ ".config/shroud/db.gpg"))) ; the default location
    (or (and (f-file? default) default)         ; if exists then use it
        (and (f-mkdir (f-dirname default)) (f-touch default)))) ; otherwise make the file and directory
  "Shroud Datastore file.
GPG Encrypted."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--config-file
  (let ((default (shroud-el--~ ".shroud")))
    (or (and (f-file? default) default)
        (and (f-mkdir (f-dirname default)) (f-touch default))))
  "Shroud Config file."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--gpg-key
  (or (and shroud-el--config-file
           (pcase-let ((`(_ ,a) (read (shroud-el--file-contents shroud-el--config-file))))
             (alist-get 'user-id a)))
      nil)
  "Shroud GPG Key."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--epg-key
  (or (and shroud-el--gpg-key
           (epg-list-keys (epg-make-context 'OpenPGP)
            shroud-el--gpg-key))
      nil)
  "Shroud EPG Key."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--file-encrypt-to
  (or (and shroud-el--gpg-key
           (concat ";; -*- epa-file-encrypt-to: (\"" shroud-el--gpg-key "\") -*-"))
      "")
  "Automatically encrypt to for this key without prompting."
  :group 'shroud
  :type 'gpg-key-id)

(defun shroud-el--write-file (reader filename)
  "Write the output of READER to FILENAME."
  (with-temp-buffer
    (if (s-suffix? ".gpg" filename)
        (progn (insert (format "%s\n" shroud-el--file-encrypt-to))))
    (insert (format "%S" (funcall reader)))
    (write-file filename)))

(defmacro shroud-el--decrypting-reader (reader &optional encoding)
  "Decrypt the result of calling READER.

If no ENCODING is specified it's assumed to be utf-8."
  `(let ((context (epg-make-context 'OpenPGP)))
     (decode-coding-string
      (epg-decrypt-string context (format "%s" (funcall ,reader)))
      (or ,encoding 'utf-8))))

(defmacro shroud-el--encrypting-reader (reader recipients &optional encoding)
  "Encrypt the result of calling READER for RECIPIENTS.

If no ENCODING is specified it's assumed to be utf-8.Write to file FILENAME."
  `(let ((context (epg-make-context 'OpenPGP)))
     (epg-encrypt-string context
                         (encode-coding-string (format "%s" (funcall ,reader))
                                               (or ,encoding 'utf-8))
                         ,recipients)))

;;; Deprecated
(defun shroud-el--read-db (db-file)
  "Decrypt and read the shroud db in DB-FILE.

Deprecated: Use `shroud-el--read-database' instead."
  (read (with-temp-buffer
          (insert-file-contents-literally (or db-file))
          (let ((context (epg-make-context 'OpenPGP)))
            (decode-coding-string
             (epg-decrypt-string context (buffer-substring-no-properties (point-min) (point-max)))
             'utf-8)))))

(defun shroud-el--read-database (db-file &optional encoding)
  "Decrypt and read the shroud db in DB-FILE.
Optional ENCODING for the file."
  (read (shroud-el--decrypting-reader (shroud-el--file-contents db-file)
                                   (or encoding 'utf-8))))

(defun shroud-el--query (q)
   "Build a query Procedure for querying Q."
  (-partial #'s-matches? q))

(defun shroud-el--entry-get (key shroud-el--entry)
  "Get KEY from SHROUD-EL--ENTRY."
  (cl-labels ((assoc-get (a b) (alist-get a b nil nil #'equal)))
    (pcase-let* ((`(,id ,contents) shroud-el--entry)
                 (`(_ . ,name) id)
                 (als `(,id ,@(rest contents))))
      (pcase key
        (`all shroud-el--entry)
        (`id name)
        (`contents (rest contents))
        (`entry `(,id (name . ,name) ,@(assoc-get 'contents shroud-el--entry)))
        (_ (assoc-get key als))))))

(defun shroud-el--entry? (entry)
  "Check if the ENTRY is a valid shroud-el--entry."
  (and (shroud-el--entry-get 'id entry)
       (shroud-el--entry-get 'contents entry) t))

(defun shroud-el--entry-exists? (entry-name db)
  "Check if the ENTRY-NAME is a valid shroud-el--entry not already present in DB."
  (and (-find (shroud-el--query (shroud-el--entry-get 'id entry-name))
              (-map (-cut shroud-el--entry-get 'id <>) db))
       t))

(defun shroud-el--run (db-file &rest args)
  "Run shroud on DB-FILE with ARGS.
Shroud entry function."
  (cl-labels ((db () (read (shroud-el--file-contents db-file)))
            (entry-name (e) (funcall (-cut shroud-el--entry-get 'id <>) e))
            (entry-names () (-map #'entry-name (db)))
            (entry-contents (e) (funcall (-cut shroud-el--entry-get 'entry <>) e))
            (assoc-get (a b) (funcall (-cut alist-get <> <> nil nil #'equal) a b))
            (check  (a b) (equal a (car b)))
            (query-car (q) (-partial #'check q))
            (find-entry (a db) (-find #'(lambda (e) (equal (entry-name e) a)) (db))))
    (pcase args
      (`("list" . ,a) (entry-names))
      (`("list" . ,a) (-filter (apply #'-orfn (-map #'shroud-el--query a)) (entry-names)))
      (`("show" ,a . ,rest)
       (let ((entry (-find #'(lambda (e) (equal (entry-name e) a)) (db))))
         (if rest (-filter (apply #'-orfn (-map #'query-car rest)) (entry-contents entry))
           entry)))
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

(defun shroud-el-run (db-file &rest args)
  "Run shroud on DB-FILE with ARGS.
Shroud user entry function."
  (pcase args
    (`(,(or "--help" "-h") . ,_) "Usage: shroud COMMAND ARGS. COMMAND may be one of:\nlist | show | remove | hide")
    (`("--version" . ,_) "1.12")
    (`("list" ,(or "--help" "-h") . ,_) "Usage: shroud list [OPTION]\nShow the names of all secrets in the database.")
    (`("show" ,(or "--help" "-h") . ,_) "Usage: shroud show [OPTION] ID [KEY ...]\nShow secret named ID.")
    (`("hide" ,(or "--help" "-h") . ,_) "Usage: shroud hide [OPTION] ID KEY=VALUE ...\nAdd a new secret named ID to the database.")
    (`("remove" ,(or "--help" "-h") . ,_) "Usage: shroud remove [OPTION] id\nRemove a secret from the database.")
    (_ (apply (-partial #'shroud-el--run db-file) args))))

(provide 'shroud-el)

;;; shroud-el.el ends here
