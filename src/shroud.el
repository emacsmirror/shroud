;;; shroud.el --- Shroud secrets

;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: https://github.com/o-nly/emacs-shroud
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
;;  Shroud is a password manager written in Guile which uses GnuPG in the
;;  backend.  See Shroud's website at https://dthompson.us/projects/shroud.html.
;;  This package is an Emacs interface to Shroud using the Buffers User
;;  Interface library.
;;
;;  Shroud stores secrets, which can be more than just passwords(think arbitrary
;;  texts) in Shroud-DB, a text-file, which is then encrypted using GnuPG.  The
;;  Shroud DB file is written using Lisp style s-expressions(a sort of markup
;;  language), and each entry looks like ((id . "id") (content
;;  . <your-data-in-alists>)).  A Shroud DB is a list of such entries.  This is
;;  intended so that the file is trivial to parse using programs, but remain
;;  perfectly readable/editable manually by hand.  (Your data is not locked up in
;;  some arbitrary database format, and the software to open it won't become
;;  unavailable.)
;;
;;  With Emacs-Shroud, you can view, copy and edit secrets right from Emacs.
;;
;;  To enter shroud's interface, M-x Shroud.  This will open up a shroud-bui
;;  buffer listing `id's of all available password entries.  In this buffer,
;;  some easy to access keybinds are provided:

;;  -----+---------------------
;;   Key | Action
;;  -----+---------------------
;;   c   | copy password
;;   I   | copy username
;;   w   | copy url
;;   a   | add new entry
;;   d   | delete entry
;;   g   | refresh buffer
;;   e   | edit entry at point
;;  -----+---------------------
;;
;;  Edit is by far the most powerful of these commands, allowing you to change
;;  anything in the entry.
;;
;; Limitations:
;; 1. currently emacs-shroud expects that your shroud db contains
;;    fields "username" "password" and "url" etc, named exactly so.  An example
;;    entry may look like ((id . "id") (contents ("username" . "foo") ("password"
;;    . "bar") ("url" . "baz")))
;; 2. Spaces are not supported in the entry contents ... ("foo" . "I like
;;   Emacs")
;; 3. Buggy or slow? If you use exwm with shroud then in some cases, the UI may
;;   hang waiting for pinentry input.
;;
;;  Configuration and Customisation
;;
;;  Shroud's configuration options can be changed in the $HOME/.shroud file.
;;  The default database is located in ~/.config/shroud/db.gpg.
;;
;;  However, Emacs-shroud also includes an elisp implementation of Shroud.  So
;;  you can begin using shroud without installing any external packages.  It can
;;  be configured to use the same defaults as Shroud like so.
;;
;;  #start ~/.emacs
;;    (setq shroud-el--user-id "yourname@example.com")
;;  #end
;;
;;  This bit will pick between shroud(if you have installed it) or
;;  shroud-el.
;;
;;  #start ~/.emacs
;;    (shroud--init)
;;  #end
;;
;;  If user-id is not set and no configuration file is found Shroud
;;  may prompt you to choose a key each time you edit the database.
;;
;;  Sample Shroud Config
;;  #start ~/.shroud
;;    '((user-id . "yourname@example.com"))
;;  #end
;;
;;  Sample Shroud Database
;;  #start ~/.config/shroud/db.gpg
;;    (((id . "my-bank") (contents  ("password" . "hackme") ("username" . "pwned") ...)) ...)
;;  #end


;;; Code:

(require 'shroud-bui)

;;;###autoload
(defalias 'shroud-ui 'shroud-bui)
;; interactively using M-x shroud
;; or (global-set-key '("C-c p") 'shroud)

;;;###autoload
(defun shroud ()
  "Completing read to copy entry password."
  (interactive)
  (and (kill-new (shroud--show-password
                  (completing-read "Shroud: " (shroud--list))))
       (message "Password copied to killring.")))

(provide 'shroud)

;;; shroud.el ends here
