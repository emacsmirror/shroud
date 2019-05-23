;;; guix.scm --- Package description for emacs-shroud

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

(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (gnu packages gnupg)
 (guix build-system emacs)
 ((guix licenses) #:prefix license:)
 (gnu packages password-utils)
 (gnu packages emacs)
 (gnu packages emacs-xyz))

(define-public emacs-shroud
  (package
   (name "emacs-shroud")
   (version "ac8b04dd50a9caa5de954b17d745ad712cc08669")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "http://github.com/o-nly/emacs-shroud.git")
       (commit version)))
     (file-name
      (git-file-name name version))
     (sha256
      (base32
       "1b8xrzyb7l3lhy1kd2gwyngg60flfha3s2rlv98y8jc44zhz2bi8"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("shroud" ,shroud)
      ("emacs-f" ,emacs-f)
      ("emacs-dash" ,emacs-dash)
      ("emacs-s" ,emacs-s)
      ("emacs-bui" ,emacs-bui)
      ("gnupg" ,gnupg)))
   (home-page "http://git.nly.info.tm:9001/shroud.git")
   (synopsis "Emacs interface for Shroud password manager")
   (description
    "This package provides functions for working with shroud
password manager using Elisp, a reader for Shroud DB, and an Buffers
User Interface for using shroud password database.")
   (license license:gpl3+)))

emacs-shroud

;;; guix.scm ends here
