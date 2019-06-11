;;; shroud-cli.el --- Shroud secrets

;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: http://git.nly.info.tm:9001/shroud.git
;;; Package-Version: 1.15
;;; Package-Requires: ((emacs "25") (epg "1.0.0") (s "1.6.0") (bui "1.2.0") (dash "2.15.0") (dash-functional "2.15.0"))
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

;;  Shroud is a password manager written in Guile which uses GnuPG in
;;  the backend.  See Shroud's website at
;;  https://dthompson.us/projects/shroud.html.  This package is an
;;  Emacs interface to Shroud using the Buffers User Interface
;;  library.
;;
;;  Shroud stores secrets as a plain text encrypted using
;;  GnuPG.  Particularly, in Lisp's S-expressions, in a form of
;;  associaton lists.  This provides the dual benefit that, the file is
;;  trivial to parse by machine, yet at the same time, is perfectly
;;  readable/editable by a human.
;;
;;  You can view, copy and edit secrets from Emacs.
;;
;;  Shroud's configuration options can be changed in the $HOME/.shroud
;;  file.  The default database is located in ~/.config/shroud/db.gpg.
;;
;;  To run M-x Shroud
;;
;;  However, Emacs-shroud also includes an elisp implementation of
;;  Shroud.  So you can begin using shroud without installing any
;;  external packages.  It can be configured to use the same defaults
;;  as Shroud like so.
;;
;;  #start ~/.emacs
;;    (setq shroud-el--database-file "~/.config/shroud/db.gpg")
;;    (setq shroud-el--config-file "~/.shroud")
;;    (setq shroud-el--gpg-key "A1761FE275883XXX")
;;  #end
;;
;;  This bit will pick between shroud(if you have installed it) or
;;  shroud-el.
;;
;;  #start ~/.emacs
;;    (shroud--init)
;;  #end
;;
;;  If gpg-key is not set and no configuration file is found Shroud
;;  may prompt you to choose a key each time you edit the database.
;;
;;  Sample Shroud Config
;;  #start ~/.shroud
;;    '((user-id . "AAOEUOEUP12323"))
;;  #end
;;
;;  Sample Shroud Database
;;  #start ~/.config/shroud/db.gpg
;;    (((id . "my-bank") (contents  ("password" . "hackme") ("username" . "pwned") ...)) ...)
;;  #end


;;; Code:

(require 'epg)
(require 'bui)
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
(defun shroud--show-entry (entry)
  "Return the results of ‘shroud--show’ ENTRY in Lisp lists."
  (mapcar #'(lambda (x) (split-string x " "))
          (mapcar #'s-collapse-whitespace
                  (split-string (shroud--show entry) "\n"))))

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


(provide 'shroud-cli)

;;; shroud-cli.el ends here
