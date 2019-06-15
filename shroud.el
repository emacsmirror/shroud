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

(require 'shroud-bui)

;;;###autoload
(defalias 'shroud 'shroud-bui)
;; interactively using M-x shroud
;; or (global-set-key '("C-c p") 'shroud)

(provide 'shroud)

;;; shroud.el ends here
