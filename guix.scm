;;; guix.scm --- Package description for emacs-shroud
;; 
;; Filename: guix.scm
;; Description: Package description for emacs-shroud
;; Author: Amar Singh
;; Maintainer: Amar Singh<nly@disroot.org>
;; Created: Thu Feb  7 13:49:08 2019 (+0530)
;; Version: 0.1
;; Package-Requires: (emacs-s, emacs-f, shroud, gnupg)
;; Last-Updated: 2019 Feb
;;           By: 
;;     Update #: 0
;; URL: http://nly.info.tm:9000/emacs/shroud.el
;; Doc URL: 
;; Keywords: shroud, password manager
;; Compatibility: Gnu Emacs 26.1
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Emacs interface for shroud password manager.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
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
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(use-modules (guix packages)
             (guix download)
             (guix build-system emacs)
             ((guix licenses) #:prefix license:)
             (gnu packages password-utils)
             (gnu packages emacs)
             (gnu packages emacs-xyz))

(define-public emacs-shroud
  (package
   (name "emacs-shroud")
   (version "0.1")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "https://github.com/o-nly/" name "/archive/release-" version ".tar.gz"))
            (sha256
             (base32
              "0jxfa2fa6r0ay9c6nf4xwz5vddb1v45cgaxkgdxxhck0l527i3yc"))))
   (build-system emacs-build-system)
   
   (propagated-inputs
    `(("emacs-f" ,emacs-f)
      ("emacs-s" ,emacs-s)
      ("emacs-bui" ,emacs-bui)
      ("shroud" ,shroud)))
   (home-page "http://nly.info.tm:9001/emacs/shroud.el")
   (synopsis "Shroud (gpg) support for Emacs")
   (description
    "This package provides functions for working with shroud password manager.")
   (license license:gpl3+)))

emacs-shroud

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; guix.scm ends here
