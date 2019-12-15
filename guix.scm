;;; guix.scm --- Package description for emacs-shroud

;; Copyright (C) 2019  Amar Singh

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Filename: guix.scm
;; Description: Package description for emacs-shroud
;; Author: Amar Singh
;; Maintainer: Amar Singh<nly@disroot.org>
;; Created: Thu Feb  7 13:49:08 2019 (+0530)
;; Version: 1.15
;; URL: http://git.nly.info.tm:9000/emacs-shroud.git
;; Keywords: shroud, password manager
;; Compatibility: Gnu Emacs 25

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(load (string-append
		 (dirname (current-filename))
		 "/guix/gnu/packages/emacs-shroud.scm"))

(use-modules (gnu packages emacs-shroud))

emacs-shroud

;;; guix.scm ends here
