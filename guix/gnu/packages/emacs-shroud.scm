;;; Package description for emacs-shroud

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

;;; Code:

(define-module (gnu packages emacs-shroud))

(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (gnu packages gnupg)
 (guix build-system gnu)
 ((guix licenses) #:prefix license:)
 (gnu packages password-utils)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages perl)
 (gnu packages texinfo))

(define-public emacs-shroud
  (package (name "emacs-shroud")
   (version "1.83.24")
   (source (origin (method git-fetch)
                   (uri (git-reference
                         (url
                          "https://git.savannah.gnu.org/git/emacs-shroud.git")
                         (commit version)))
                   (file-name (git-file-name name version))
                   (sha256
                    (base32
                     "016j5f3igc8k84cq16gr95x868ddlav52d5fslv8jakcjaqkhxg5"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (delete 'configure)
                 (delete 'check)
                 (replace 'install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (el-dir (string-append out "/share/emacs/site-lisp"))
                            (doc (string-append
                                  out "/share/doc/emacs-shroud-" ,version))
                            (info (string-append out "/share/info")))
                       (define (copy-to-dir dir files)
                         (for-each (lambda (f)
                                     (install-file f dir))
                                   files))
                       (with-directory-excursion "doc"
                         (install-file "emacs-shroud.info" info))
                       (copy-to-dir doc '("README.org" "TODO.org" "art/shroud.svg"))
                       (copy-to-dir el-dir (find-files "./bin/emacs/" ".\\el"))
                       #t))))))
   (native-inputs
    `(("texinfo" ,texinfo)
      ("perl" ,perl)
      ("emacs-minimal" ,emacs-minimal)))
   (propagated-inputs
    `(("shroud" ,shroud)
      ("emacs-f" ,emacs-f)
      ("emacs-dash" ,emacs-dash)
      ("emacs-s" ,emacs-s)
      ("emacs-bui" ,emacs-bui)
      ("gnupg" ,gnupg)))
   (home-page "https://www.nongnu.org/emacs-shroud")
   (synopsis "Emacs interface for Shroud password manager")
   (description
    "This package provides functions for working with shroud password
manager using Elisp, a reader for Shroud DB, and an Buffers User
Interface for using shroud password database.")
   (license license:gpl3+)))

;;; file ends here
