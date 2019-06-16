;;; shroud-cli.el --- Shroud secrets

;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: https://github.com/o-nly/emacs-shroud
;;; Package-Version: 1.15
;;; Package-Requires: ((emacs "25") (s "1.6.0") (dash "2.12.0") (dash-functional "2.12.0"))
;;; Keywords: tools, password

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Elisp bindings to shroud cli or compatible interfaces.

;;; Code:

(require 's)
(require 'dash)
(require 'dash-functional)
(require 'shroud-el)

(defgroup shroud '()
  "Interface for shroud password manager"
  :prefix "shroud-"
  :group 'shroud)

(defcustom shroud-password-length 8
  "Default password length."
  :group 'shroud
  :type 'number)

(defcustom shroud-executable (executable-find "shroud")
  "Shroud executable."
  :group 'shroud
  :type 'executable)

(defcustom shroud-database-file (concat (getenv "HOME")
					"/.config/shroud/db.gpg")
  "Shroud Datastore file.

GPG Encrypted."
  :group 'shroud
  :type 'file)

(defcustom shroud-timeout (or (getenv "SHROUD_CLIPBOARD_TIMEOUT")
      45)
  "Number of seconds to wait before clearing the password."
  :group 'shroud
  :type 'integer)

;;; use shroud--run instead.
(defun shroud--internal-old (shroud-command &rest args)
  "Internal shroud helper function.
Execute SHROUD-COMMAND with &rest ARGS."
  (s-trim (shell-command-to-string
            (mapconcat 'identity
                       (cons shroud-executable
                             (cons shroud-command
                                   (delq nil args)))
                       " "))))

(defun shroud--run-internal (&rest args)
  "Run the shroud commands with ARGS.
Nil arguments will be ignored.  Returns the output on success,  or
  outputs error messasge on failure."
  (with-temp-buffer
    (let* ((tempfile (make-temp-file ""))
           (exit-code
            (apply 'call-process
                   (append
                    (list shroud-executable nil (list t tempfile) nil)
                    (delq nil args)))))
      (unless (zerop exit-code)
        (erase-buffer)
        (insert-file-contents tempfile))
      (delete-file tempfile)
      (if (zerop exit-code)
          (s-trim (buffer-string))
        (error (s-trim (buffer-string)))))))

(defun shroud--init ()
  "Run shroud on ARGS."
  (if shroud-executable
      (defalias 'shroud--run 'shroud--run-internal)
    (defalias 'shroud--run (-partial #'shroud-el-run (or shroud-el--database-file
                                                         shroud-database-file)))))

(shroud--init)

;;; Help
(defun shroud--help (&rest sub-entry)
  "Return shroud help strings.
SUB-ENTRY is passed straight to shroud."
  (apply #'shroud--run `(,@sub-entry "--help")))

(defun shroud--help--list ()
  "Return help strings for shroud list."
  (shroud--help "list"))

(defun shroud--help--remove ()
  "Return help strings for shroud remove."
  (shroud--help "remove"))

(defun shroud--help--hide ()
  "Return help strings for shroud hide."
  (shroud--help "hide"))

(defun shroud--help--show ()
  "Return help strings for shroud show."
  (shroud--help "show"))

(defun shroud--version ()
  "Return shroud version."
  (shroud--run "--version"))

;;; List Entries
(defun shroud--list ()
  "Return the output of shroud list.
ARGS are passed straight to shroud."
  (split-string  (shroud--run "list") "\n"))

;;; Hide secrets
(defun shroud--hide (&rest args)
  "Return the output of shroud hide.
ARGS are passed straight to shroud."
  (apply #'shroud--run "hide" args))

(defun shroud--hide-edit (&rest args)
  "Return the output of shroud edit.
ARGS are passed straight to shroud."
  (apply #'shroud--hide "--edit" args))
;;; shroud hide edit entry password
;;; shroud hide edit entry username
;;; shroud hide edit add entry new-entry value
;;; shroud hide edit

(defun shroud--show (entry &rest args)
  "Return the output of shroud show ENTRY.
if ARGS are nil, shroud will show you all sub-entries.
Otherwise, you can pass the ARGS as STRING."
  (apply #'shroud--run "show" entry args))

;;; Bug when entries may contain empty entries or newlines in entries
(defun shroud--show-entry (entry &optional full?)
  "Return the results of ‘shroud--show’ ENTRY in Lisp lists.
If OPTIONAL FULL? is t then return a full entry."
  (let ((res (mapcar #'(lambda (ls) (cons (car ls) (cadr ls)))
                     (mapcar #'(lambda (x) (split-string x " "))
                             (mapcar #'s-collapse-whitespace
                                     (split-string (shroud--show entry) "\n"))))))
    (if (not full?) res
      `((id . ,entry) (contents . ,res)))))

(defun shroud--show-sub-entries (entry &rest sub-entry)
  "Return the output of shroud show ENTRY.
if SUB-ENTRY are nil, shroud will show you all sub-entries.
Otherwise, you can pass the ARGS as STRING."
  (apply #'shroud--show entry sub-entry))

(defun shroud--show-clipboard (entry &rest sub-entries)
  "Add the ENTRY and SUB-ENTRIES to clipboard."
  (apply #'shroud--show "--clipboard" entry sub-entries))

(defun shroud--show-username (entry)
  "Show the username for given ENTRY."
  (shroud--show entry "username"))

(defun shroud--show-password (entry)
  "Show the password for given ENTRY."
  (shroud--show entry "password"))

(defun shroud--show-url (entry)
  "Show the url for given ENTRY."
  (shroud--show entry "url"))

(defun shroud--remove (entry)
  "Shroud remove ENTRY."
  (shroud--run "remove" entry))

(defmacro shroud--query (q)
  "Apply s-matches partially to Q."
  `(lambda (s) (s-matches? ,q s)))

(defun shroud--find (entry)
  "Shroud find ENTRY.

Returns a list of matches."
  (-filter (shroud--query entry) (shroud--list)))

(defalias 'shroud-cli--input-string->entry
  'shroud-el--input-string->shroud-entry)

(defalias 'shroud-cli--entry->input-string
  'shroud-el--entry->input-string)

(defalias 'shroud-cli--entry->output-string
  'shroud-el--entry->output-string)

(defalias 'shroud-cli--entry-get 'shroud-el--entry-get)

(defun shroud-cli--entry-name->input-string (e)
  "Parse entry E into a Shroud CLI compatible string."
  (shroud-cli--entry->input-string (shroud--show-entry e t)))

(defun shroud-cli--entry-name->entry-sexp (e)
  "Return a shroud-entry given entry name E."
  (shroud--show-entry e t))

(provide 'shroud-cli)

;;; shroud-cli.el ends here
