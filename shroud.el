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
  "Does not work, atleast, in emacs exwm."
  (apply #'shroud--show "--clipboard" entry sub-entries))
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

;;; Minor mode map will contain the keyboard shortcuts for shroud-minor-mode.
(defvar shroud-minor-mode-map (make-sparse-keymap "shroud-minor-mode-map"))

;;; This procedure prints the available entries in shroud in a split window.
(defun nly/shroud ()
  "Open a *shroud* buffer in new window listing available entries.

Activates READ-ONLY-MODE and SHROUD-MINOR-MODE."
  (interactive)
  (progn
    (switch-to-buffer-other-window "*shroud*")
    (read-only-mode -1)
    (erase-buffer)
    (insert
     (propertize
      "Keybinds: n -> next line | p -> previous-line | u -> copy-username
s -> copy-password | l -> copy-url | q -> quit\n\n"
      :face '(:foreground "green")))
    (shroud-minor-mode t)
    (insert  (mapconcat
              'identity
              (shroud--list)
              "\n"))
    (read-only-mode t)))
;;; Define a minor mode to be enabled in this new buffer so that i can
;;; get the password or username or url with a single shortcut. The
;;; minor mode will make it easier to toggle between the shroud shortcuts.
(define-minor-mode shroud-minor-mode
  "Shroud mode which binds easy shortcuts to shroud commands."
  :mode nil
  :init-value nil
  :global nil
  :lighter " Shroud"
  :keymap shroud-minor-mode-map
  )
;;; Enable the mode for testing.
;;(shroud-minor-mode t)
;;; A helper function which will help us get the string of the current
;;; entry. We'd like to close the buffer after we have performed the
;;; operation. Atleast provides some privacy from
;;; peeking-over-the-shoulder(tm) attacks.
(defun nly/shroud-entry ()
  "Get the current shroud entry."
  (save-excursion
    (let* ((p1 (progn (beginning-of-line) (point)))
           (p2 (progn (end-of-line) (point)))
           (text (buffer-substring-no-properties p2 p1)))
      (kill-buffer "*shroud*")
      (delete-window)
      text)))

;;; We would like to atleast have the ability to get the password,
;;; url, or username of the current entry, one at a time. For that we
;;; are defining some procedures which can then be bound to single key
;;; shortcuts.
(defun nly/shroud-get-password ()
  "Copy password to clipboard for current entry."
  (interactive)
  (kill-new (shroud--show-password (nly/shroud-entry))))
(defun nly/shroud-get-url ()
  "Copy password to clipboard for current entry."
  (interactive)
  (kill-new (shroud--show-url (nly/shroud-entry))))
(defun nly/shroud-get-username ()
  "Copy password to clipboard for current entry."
  (interactive)
  (kill-new (shroud--show-username (nly/shroud-entry))))

;;; Bind the desirable functionality to easy to understand keys. Bug:
;;; there is no documentation for these in C-h m; `describe-mode'.
(define-key shroud-minor-mode-map (kbd "n")
  'forward-line)
(define-key shroud-minor-mode-map (kbd "f")
  'forward-char)
(define-key shroud-minor-mode-map (kbd "b")
  'backward-char)
(define-key shroud-minor-mode-map (kbd "p")
  'previous-line)
(define-key shroud-minor-mode-map (kbd "s")
  'nly/shroud-get-password)
(define-key shroud-minor-mode-map (kbd "u")
  'nly/shroud-get-username)
(define-key shroud-minor-mode-map (kbd "l")
  'nly/shroud-get-url)
(define-key shroud-minor-mode-map (kbd "q")
  'delete-window)

;;; Warning this module performs keybinds that you might not
;; expect. You can ofcourse bind it to something else or call it
;; interactively using M-x.
;; (global-set-key '("C-c p") 'nly/shroud)

(provide 'shroud)
;;; shroud.el ends here
