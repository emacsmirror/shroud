;;; shroud-bui.el --- Shroud secrets

;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: https://github.com/o-nly/emacs-shroud
;;; Package-Version: 1.15
;;; Package-Requires: ((emacs "25") (s "1.6.0") (bui "1.2.0") (dash "2.12.0") (dash-functional "2.12.0"))
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

;;; It'd be awesome to implement other UIs too, `completing-read', hydra,
;;; transient etc.

;;; this module cannot depend on shroud.el or shroud-el.
;;; TODO:
;; 1. functional style TODO
;; 2. require forms DONE
;; 3. package metadata/commentss TODO
;; 4. a clean interface to define shroud bui TODO
;; 5. custom field formats TODO

;;; Code:
(require 'bui)
(require 'dash)
(require 'dash-functional)
(require 'shroud-cli)

;; New BUI
(bui-define-groups shroud-bui
  :parent-group tools
  :parent-faces-group faces
  :group-doc "Settings for '\\[shroud-bui]' command."
  :faces-group-doc "Faces for '\\[shroud-bui]' command.")

(defun shroud-bui-find-entries (&optional search-type &rest search-values)
  "Filter results from a LST of entries.

If no SEARCH-TYPE is provided, return the list as is, otherwise Flter the
list using SEARCH-VALUES as predicates."
  (let ((lst (shroud--list)))
    (if (not search-type) lst
      (pcase search-type
        ('all lst)
        ('pred (-filter (apply #'-orfn search-values) lst))))))

(defun shroud-bui-entry->entry (a)
  "Given an entry name A, return an alist suitable for `shroud-bui'."
  `((name . ,a)
    (id . ,a)))

(defun shroud-bui-get-entries (&rest args)
  "Get a list of entry names as strings.
Pass in ARGS to `shroud--list'."
  (mapcar #'shroud-bui-entry->entry
          (apply #'shroud-bui-find-entries args)))

(bui-define-interface shroud list
  :buffer-name "*Shroud*"
  :get-entries-function 'shroud-bui-get-entries
  :format '((name nil 30 t))
  :sort-key '(name))

(let ((map shroud-list-mode-map))
  (define-key map (kbd "c") #'shroud-bui-copy-entry-pass)
  (define-key map (kbd "d") #'shroud-bui-remove-entry)
  (define-key map (kbd "e") #'shroud-bui-edit-entry)
  (define-key map (kbd "a") #'shroud-bui-add-entry)
  (define-key map (kbd "w") #'shroud-bui-copy-entry-url)
  (define-key map (kbd "I") #'shroud-bui-copy-entry-username))

(defun shroud-bui-list--cmd (cmd msg)
  "Execute CMD with `bui-list-current-id' as argument and show MSG."
  (and (kill-new (funcall cmd (bui-list-current-id)))
       (message "%s" msg)))

(defun shroud-bui-copy-entry-pass ()
  "Copy current entry password."
  (interactive)
  (shroud-bui-list--cmd #'shroud--show-password "Copied password."))

(defun shroud-bui-copy-entry-url ()
  "Copy current entry url."
  (interactive)
  (shroud-bui-list--cmd #'shroud--show-url "Copied uri."))

(defun shroud-bui-copy-entry-username ()
  "Copy current entry username."
  (interactive)
  (shroud-bui-list--cmd #'shroud--show-username "Copied username."))

(defun shroud-bui-remove-entry ()
  "Remove current entry."
  (interactive)
  (and (shroud--remove (bui-list-current-id))
       (message "Entry deleted.")))

(defvar shroud-bui-edit-entry-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'shroud-bui-save-entry)
    map)
  "Shroud minor mode for editing a Shroud entry.
Provides a keybind to save the current sexp to shroud db, checking if it's
a valid entry.")

;;; Editing is a bit more involved
(define-minor-mode shroud-bui-edit-entry-minor-mode
  "Minor mode for editing shroud entry"
  :init-value nil
  :group 'shroud
  :lighter " Shroud-Bui-edit"
  :require 'shroud
  :keymap shroud-bui-edit-entry-minor-mode-map
  )

(defcustom shroud-bui--alist
  '((id . "id")
    (contents .
              (("username" . "")
               ("password" . "")
               ("url" . "")
               ("notes" . ""))))
  "Shroud sample entry sexp."
  :type 'sexp)

(defun shroud-bui--hide-alist (exp)
  "Shroud save an entry EXP."
  (let ((res (shroud-cli--entry->input-string exp t)))
    (if (shroud--find (alist-get 'id exp))
        (apply #'shroud--hide-edit res)
      (apply #'shroud--hide res))))

(defun shroud-bui-save-entry (&optional exp)
  "Save an entry EXP to shroud db."
  (interactive)
  (and (shroud-bui--hide-alist
        (or exp (with-current-buffer (current-buffer)
                  (read (buffer-string)))))
       (message "Entry updated.")))

(defun shroud-bui--make-entry-buffer (entry)
  "Make a `buffer-name' to edit ENTRY."
  (concat "*shroud-bui-edit*-" entry))

(defun shroud-bui--make-buffer-entry (buffer)
  "Given Shroud-edit- BUFFER, return the corresponding entry-name."
  (car (s-split "<" (substring buffer (length "*shroud-bui-edit*-")))))

(defun shroud-bui-list-edit-current-entry--internal (&optional entry)
  "Shroud-edit- bui-list-current-id.
If optional ENTRY is specified then edit that instead."
  (interactive)
  (let* ((entry (or entry (bui-list-current-id)))
         (buffer (generate-new-buffer-name (shroud-bui--make-entry-buffer entry)))
         (entry-sexp (shroud--show-entry entry)))
    (and
     (progn
       ;; open the entry in a new buffer
       ;; allow the user to make changes
       ;; when user saves C-x C-s then
       ;; 1. save the entry 2. discard the buffer
       (generate-new-buffer buffer)
       (switch-to-buffer-other-window buffer)
       (with-current-buffer buffer
         (emacs-lisp-mode)
         (shroud-bui-edit-entry-minor-mode)
         (insert (format "%S" (or entry-sexp shroud-bui--alist)))))
     (message (format "Shroud: editing %s , when finished Press C-c C-s" entry)))))

(defun shroud-bui-edit-entry ()
  "Edit the current entry in Shroud list BUI."
  (interactive)
  (let ((entry (bui-list-current-id)))
    (and (shroud-bui-list-edit-current-entry--internal entry)
         (message "Edit: %s" entry))))

(defun shroud-bui-list-add-entry--internal (&optional entry)
  "Add new entry.
If Optional ENTRY is provided use that."
  (interactive)
  (let* ((buffer (generate-new-buffer-name (shroud-bui--make-entry-buffer "new"))))
    (and
     (progn
       ;; open the entry in a new buffer
       ;; allow the user to make changes
       ;; when user saves C-x C-s then
       ;; 1. save the entry 2. discard the buffer
       (generate-new-buffer buffer)
       (switch-to-buffer-other-window buffer)
       (with-current-buffer buffer
         (emacs-lisp-mode)
         (shroud-bui-edit-entry-minor-mode)
         (insert (format "%S" shroud-bui--alist))))
     (message (format "Shroud: Adding new entry, when finished Press C-c C-s")))))

(defun shroud-bui-add-entry ()
  "Add new entry."
  (interactive)
  (and (shroud-bui-list-add-entry--internal)
       (message (concat "Add: new entry"))))

;;;###autoload
(defun shroud-bui ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'shroud 'list))

(provide 'shroud-bui)

;;; shroud-bui.el ends here
