;; Copyright (C) 2019  Amar M. Singh

;; This file is part of emacs-shroud.

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

(load (string-append
		 (dirname (current-filename))
		 "/guix/gnu/packages/emacs-shroud.scm"))

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (gnu packages emacs-shroud))

(define %source-dir (dirname (current-filename)))

(define emacs-shroud-local
  (package (inherit emacs-shroud)
            (name "emacs-shroud")
            (version "git")
            (source (local-file %source-dir
                                #:recursive? #t
                                #:select? (git-predicate %source-dir)))))

emacs-shroud-local

;;; file ends here
