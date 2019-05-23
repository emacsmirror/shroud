;;; shroud-el.el --- Elisp implementation of Shroud

;;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: http://git.nly.info.tm:9001/shroud.git
;;; Package-Version: 1.15
;;; Keywords: tools, password management
;;; Package-Requires: ((epg "1.0.0") (emacs "25") (s "1.6.0") (dash "2.15.0") (dash-functional "2.15.0"))

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

(defun shroud-el--~ (file)
  "Find FILE in user HOME."
    (concat (getenv "HOME") "/" file))

(defun shroud-el--file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun shroud-el--read-config (key filename)
  (let ((cfg (read (shroud-el--file-contents filename))))
    (pcase-let*
        ((`(quote ,contents) cfg)
         (user-id (alist-get 'user-id contents)))
      (pcase key
        ('user-id user-id)
        ('contents contents)
        (t "nothing")))))

(defcustom shroud-el--database-file nil
  "Shroud Datastore file.
GPG Encrypted."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--gpg-key nil
  "Shroud GPG Key."
  :group 'shroud
  :type 'file)

(defcustom shroud-el--config-file (shroud-el--~ ".shroud")
  "Shroud Config file."
  :group 'shroud
  :type 'file)

(defun shroud-el--write-file (reader filename)
  "Write the output of READER to FILENAME."
  (with-temp-buffer
    (if (s-suffix? ".gpg" filename)
        (cl-labels ((fmt (s) (format ";; -*- epa-file-encrypt-to: (\"%s\") -*-\n" s)))
          (insert (cond
                   (shroud-el--gpg-key
                    (fmt shroud-el--gpg-key))
                   ((file-exists-p shroud-el--config-file)
                    (fmt (shroud-el--read-config 'user-id
                                                 shroud-el--config-file)))
                   (t "\n")))))
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

(defun shroud-el--entry-get (key shroud-entry)
  "Get KEY from SHROUD-EL--ENTRY."
  (cl-labels ((assoc-get (a b) (alist-get a b nil nil #'equal)))
    (pcase-let*
        ((`((id . ,id) (contents . ,contents)) shroud-entry)
         (entry `((id . ,id) (name . ,id) ,@contents)))
      (pcase key
        (`all shroud-entry)
        (`id id)
        (`contents contents)
        (`entry entry)
        (_ (assoc-get key entry))))))

(defun shroud-el--entry? (entry)
  "Check if the ENTRY is a valid shroud-el--entry."
  (pcase entry
    (`((id . ,id) (contents . ,contents)) t)))

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
            (entry-contents (e) (funcall (-cut shroud-el--entry-get 'entry <>) e))
            (check  (a b) (equal a (car b)))
            (query-car (q) (-partial #'check q))
            (query-name (x y) (s-matches? x (entry-name y)))
            (make-query (q) (-partial #'query-name q)))
    (pcase args
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
    (_  (let ((res (apply (-partial #'shroud-el--run db-file) args)))
          (cond
           ((shroud-el--entry? (car res)) (mapconcat 'identity (-map (-cut shroud-el--entry-get 'id <>) res) "\n"))
           ((shroud-el--entry? res) (s-join "\n"
                                            (-map #'(lambda (p) (concat (car p) " " (cdr p)))
                                                  (shroud-el--entry-get 'contents res))))
           (res (s-join "\n" (-map #'cdr res))))))))

(provide 'shroud-el)

;;; shroud-el.el ends here
