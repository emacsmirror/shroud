;;; shroud.el --- shroud interface for emacs.        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Amar Singh

;; Author: Amar Singh <nly@disroot.org>
;; Version: 0.1
;; Keywords: password-manager, guile, guile-shroud
;; Package-Requires: ((f) (s) (with-editor))

;; This file is not part of GNU Emacs.

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

;; Shroud is a simple password manager using gnupg to encrypt plain
;; scheme expressions.  This package provides functions for working
;; with shroud.
;;
;; http://nly.info.tm:9000/emacs/shroud.el

;;; Code:

(require 's)
(require 'f)

(defgroup shroud '()
  "Emacs mode for shroud password manager"
  :prefix "shroud-"
  :group 'shroud)
(defcustom shroud-password-length 8
  "Default password length."
  :group 'shroud
  :type 'number)
(defvar shroud-executable
  (executable-find "shroud")
  "Shroud executable.")
(defvar shroud-timeout-timer nil
  "Timer for clearing the clipboard.")
(defun shroud-timeout ()
  "Number of seconds to wait before clearing the password."
  (if (getenv "SHROUD_CLIPBOARD_TIMEOUT")
      (string-to-number (getenv "SHROUD_CLIPBOARD_TIMEOUT"))
    45))

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
          (s-chomp (buffer-string))
        (error (s-chomp (buffer-string)))))))
(defalias 'shroud--run 'shroud--run-internal)

;;; use shroud--run instead.
(defun shroud--internal-old (shroud-command &rest args)
  "Internal shroud helper function.
Execute SHROUD-COMMAND with &rest ARGS."
  (s-chomp (shell-command-to-string
            (mapconcat 'identity
                       (cons shroud-executable
                             (cons shroud-command
                                   (delq nil args)))
                       " "))))

;;; these are working
(defun shroud--help (&rest sub-entry)
  "Return shroud help strings.
SUB-ENTRY is passed straight to shroud."
  (apply #'shroud--run (car sub-entry) "--help" '()))

(defun shroud--help--list ()
  "Return shroud help strings."
  (shroud--help "list"))
(defun shroud--help--remove ()
  "Return shroud help strings."
  (shroud--help "remove"))
(defun shroud--help--hide ()
  "Return shroud help strings."
  (shroud--help "hide"))
(defun shroud--help--show ()
  "Return shroud help strings."
  (shroud--help "show"))

(defun shroud--version ()
  "Return shroud help strings.
ARGS are passed straight to shroud."
  (shroud--run "--version"))

(defun shroud--list ()
  "Return the output of shroud list.
ARGS are passed straight to shroud."
  (s-split "\n" (shroud--run "list")))

(defun shroud--hide (&rest args)
  "Return the output of shroud list.
ARGS are passed straight to shroud."
  (apply #'shroud--run "hide" args))
(defun shroud--hide-edit (&rest args)
  "Return the output of shroud list.
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
(defun shroud--show-sub-entries (entry &rest sub-entry)
  "Return the output of shroud show ENTRY.
if SUB-ENTRY are nil, shroud will show you all sub-entries.
Otherwise, you can pass the ARGS as STRING."
  (apply #'shroud--show entry sub-entry))
(defun shroud--show-clipboard (entry &rest sub-entries)
  "Does not work, atleast, in EMACS exwm.  ENTRY.  SUB-ENTRIES."
  (apply #'shroud--show "--clipboard" entry sub-entries))
;;; Bug when entries may contain empty entries or newlines in entries
(defun shroud--show-entry (entry)
  "Return the results of shroud--show ENTRY in Lisp lists."
  (mapcar #'(lambda (x) (s-split " " x))
          (mapcar 's-collapse-whitespace
                  (s-split "\n" (shroud--show entry)))))
(defun shroud--show-username (entry)
  "Show the password for given ENTRY."
  (shroud--show entry "username"))
(defun shroud--show-password (entry)
  "Show the password for given ENTRY."
  (shroud--show entry "password"))
(defun shroud--show-url (entry)
  "Show the password for given ENTRY."
  (shroud--show entry "url"))

(defun shroud--remove (entry)
  "Shroud remove ENTRY."
  (apply 'shroud--run `("remove" ,entry)))
;;; So, we have most of the commands that we will need to use bound to
;;; very friendly elisp functions. Notably missing is clipboard clear
;;; functionality.

;;; However, since I am already depending on shroud, it's better to
;;; slowly improve upon the broken application rather wait for myself
;;; to get the motivation(tm) to do it properly.

;;; I like the popup buffer UI a lot. I think a good UI would still be
;;; a helm UI but i still don't fully understand how it might work.

;;; Let's implement what we know already, We need a popup buffer which
;;; will show the available entries in shroud.  First course of
;;; action, define a procedure to output all shroud entries in a new
;;; buffer. Then, We will have a shroud-minor-mode-map we will add the
;;; keybindings to, so that we can quickly execute commands on
;;; entries. Commands like adding password, url, or either username to
;;; the kill-ring.

;;; Minor mode map will contain the keyboard shortcuts for
;;; `shroud-minor-mode'.It may not be necessary after we have a better
;;; Interface like BUI.
;;; This procedure prints the available entries in shroud in a split
;;; window.  The ad-hoc buffer interface we created earlier is not
;;; sufficient, it uses up too much space, messes up the previous
;;; window layout. We'd like to use something more convenient, and at
;;; the same time, avoid the work that comes with it. The author of
;;; this package is already familiar with Buffer User Interface (BUI)
;;; used in guix.el. So, it would be a fine first choice. We want a
;;; buffer to display the available password entries, and though, bui
;;; should provide us to display more details, for the privacy context
;;; of our application, we'd like to only show the Title, and perhaps
;;; the URL? Depending on how risky you are feeling, you could display
;;; even emails/username(!) for peek-over-the-shoulder security
;;; attacks. But make no mistake, this package is by no means secure,
;;; as it stores the password as plain string in the kill
;;; ring. Lazy...  err, lack of incentive.

;;; Load the bui library
(require 'bui)

;;; Hug it, I'll drop the nly/ prefix, it just seems silly now.
;;; The entry point to the shroud BUI.
;;; Let's just read directly from the db.gpg file using elisp
(defun shroud-entries ()
  "Format the shroud db to something suitable for BUI."
  ;;(mapcar 'shroud-entry (shroud--list))
  ;; This function can read a shroud db from a file.
  (let* ((db (shroud--read-db)))
    (defun first (x) (car x))
    (defun rest (x) (cdr x))
    (defun flatten-contents (x) (rest (first x)))
    (defun name-from-id (x)
      (cons (if (equal (first x) 'id)
                'name
              (first x))
            (rest x)))
    (mapcar
     #'(lambda (x) (cons (first x)
                    (cons (name-from-id (first x))
                          (flatten-contents (rest x)))))
     db)))

;; Individually reading entries is painfully slow
(defun shroud-bui ()
  "Display a list of entries in the shroud db."
  (interactive)
  (bui-get-display-entries 'shroud-entries 'list))
;;; This is the interface for defining a BUI interface. It has a
;;; simple declarative syntax and a clean seperation of processes. For
;;; example, the BUI requires the entries to be formatted, you can
;;; define the format within this interface, and then leisurely define
;;; a procedure, externally, which may "massage" your data into the
;;; correct format.
(bui-define-interface shroud-entries list
  :buffer-name "*Shroud*"
  :get-entries-function 'shroud-entries
  :format
  '((name nil 30 t)
    (password nil 30 t)
    (username nil 8 t)
    (url nil 8 t)
    (notes nil 8 t))
  :sort-key '(name))

;;; Alright, so it appears shroud is very limited in terms of
;;; functionality and i can probably do much better if i simply use
;;; the elisp features to read and parse the file. Shroud provides
;;; another interface to the database, which then makes three
;;; interfaces, plain text; after decryption, shroud cli, shroud.el,
;;; and pure elisp interface. At this point only the data structure is
;;; important.  So, emacs has features to decrypt a file, decrypt
;;; buffers and whatnot. I have a feeling this is also going to be a
;;; bit ad-hoc, which scares me. Ad-hoc code is never portable or
;;; doesnt last as long. Nevertheless it's much better than limiting
;;; myself to the very limited cli interface, through which all elisp
;;; code has to go anyway. Alternatively can i use a guile daemon? So
;;; enraging arghh!

;;; Elisp reader for shroud db.gpg
(defun shroud--read-db (&optional db-file)
  "Decrypt and read the shroud db.  By default it's the db.gpg file.

Optionally DB-FILE is the file you want to read."
  (read (with-temp-buffer
          (insert-file-contents-literally (if db-file db-file (concat (getenv "HOME")
                                                       "/.config/shroud/db.gpg")))
          (let ((context (epg-make-context 'OpenPGP)))
            (decode-coding-string
             (epg-decrypt-string context (buffer-substring-no-properties (point-min) (point-max)))
             'utf-8)))))

;;; Just reading the database is not enough, we need to slighly
;;; massage the data so that it's usable by BUI.

;;; Define a minor mode to be enabled in this new buffer so that i can
;;; get the password or username or url with a single shortcut. The
;;; minor mode will make it easier to toggle between the shroud shortcuts.
;;; Enable the mode for testing.
;;(shroud-minor-mode t)
;;; A helper function which will help us get the string of the current
;;; entry. We'd like to close the buffer after we have performed the
;;; operation. Atleast provides some privacy from
;;; peeking-over-the-shoulder(tm) attacks.

;;; We would like to atleast have the ability to get the password,
;;; url, or username of the current entry, one at a time. For that we
;;; are defining some procedures which can then be bound to single key
;;; shortcuts.

;;; Bind the desirable functionality to easy to understand keys. Bug:
;;; there is no documentation for these in C-h m; `describe-mode'.
;;; Warning this module performs keybinds that you might not
;; expect. You can ofcourse bind it to something else or call it
;; interactively using M-x.
;; (global-set-key '("C-c p") 'shroud)

(provide 'shroud)

;;; shroud.el ends here
