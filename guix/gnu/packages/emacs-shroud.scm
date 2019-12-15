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
 (guix build-system emacs)
 ((guix licenses) #:prefix license:)
 (gnu packages password-utils)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages perl)
 (gnu packages texinfo))

(define-public emacs-shroud
  (package (name "emacs-shroud")
   (version "1.83.4")
   (source (origin (method git-fetch)
                   (uri (git-reference
                         (url
                          "https://git.savannah.gnu.org/git/emacs-shroud.git")
                         (commit version)))
                   (file-name (git-file-name name version))
                   (sha256
                    (base32
                     "1yvdjx0kp4y8w5yz2cbqq9n6xl5splvmsyyx8ld1xv0q1c9872nf"))))
   (build-system emacs-build-system)
   (native-inputs
    `(("perl" ,perl)
      ("texinfo" ,texinfo)))
   (propagated-inputs
    `(("shroud" ,shroud)
      ("emacs-f" ,emacs-f)
      ("emacs-dash" ,emacs-dash)
      ("emacs-s" ,emacs-s)
      ("emacs-bui" ,emacs-bui)
      ("gnupg" ,gnupg)))
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (add-after 'build 'build-docs
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; (system "make -C ./doc")
                     ;; (chdir "..")
                     #t)))))
   (home-page "https://www.nongnu.org/emacs-shroud")
   (synopsis "Emacs interface for Shroud password manager")
   (description
    "This package provides functions for working with shroud password
manager using Elisp, a reader for Shroud DB, and an Buffers User
Interface for using shroud password database.")
   (license license:gpl3+)))

;;; file ends here
