;;; shroud-test.el --- Buffers User Interface for shroud

;;; Copyright (C) 2019  Amar Singh

;;; Author: Amar Singh <nly@disroot.org>
;;; Homepage: http://git.nly.info.tm:9001/shroud.git
;;; Package-Version: 1.15
;;; Keywords: tools, tests
;;; Package-Requires: ((emacs "25"))

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO:
;; 1. handle blocking, evertime shroud--run is called Emacs could hang. TODO
;; the blocking situation: anytimo shroud--run is called, the process
;; may continue immediately if the GPG-agent is already authorised,
;; otherwise it may prompt for a password. but wait you are already
;; in a process, so you can't enter the password until you kill the
;; current process, and try again.

;;; 2. logo TODO
;;; 3. Auth-source backend for shroud TODO
;;; 4. shroud--hide TODO
;;; 5. shroud--hide-edit TODO
;;; 6. shroud--show --clipboard TODO

;; Code:

;;; Tests:
;;; 1. Make a test database file DONE
;;; 2. Make a test config file DONE
;;; 3. (setq  shroud-executable nil) (shroud--init) DONE
;;; 4.a Check `shroud-el--run' DONE
;;; 4.b Check `shroud-el-run' DONE/PENDING hide and hide --edit and show --clipboard
;;; 4.c Check `shroud-ui' DONE

;; (push (directory-file-name "~/git/shroud.el/") load-path)
(require 'shroud)
(require 'shroud-el)

;;; Test Setup: Database
(defun shroud-test--make-entry (id contents)
  (if (and (listp (car contents)) (not (listp (cdr (car contents)))))
      `((id . ,id) (contents ,@contents))
    "contents shroud be an alist"))

(defvar shroud-test--e1 "try1")
(defvar shroud-test--e2 "/dev/null")
(defvar shroud-test--e3 "bbc")
(defvar shroud-test--e4 "failsafe")

(defvar shroud-test-e1 (shroud-test--make-entry shroud-test--e1 '(("password" . "iCanHazCheez") ("username" . "n00b"))))
(defvar shroud-test-e2 (shroud-test--make-entry shroud-test--e2 '(("password" . "edef4bd1dbf12c26d00ac8e50aac797f662e49bc") ("email" . "leet@xyz.sh"))))
(defvar shroud-test-e3 (shroud-test--make-entry shroud-test--e3 '(("url" . "news.com") ("username" . "john-tho"))))
(defvar shroud-test-e4 (shroud-test--make-entry shroud-test--e4 '(("password" . "1234") ("username" . "joe"))))

(defvar shroud-test--db
  (list
   shroud-test-e1
   shroud-test-e2
   shroud-test-e3
   ))

(defun shroud-test--make-config (gpg-key)
  `(quote ((user-id . ,gpg-key))))

(defvar shroud-test--config (shroud-test--make-config "a"))

(defun shroud-test-write (item &optional suffix enc)
  (let ((file (concat "/tmp/" (make-temp-name (concat "shroud-test-" suffix)) (if enc ".gpg"))))
    (shroud-el--write-file (lambda () item) file)
    file))

(defvar shroud-test-config (shroud-test-write shroud-test--config "cfg"))

(defvar shroud-test-db (shroud-test-write shroud-test--db "db" t))

;;; Test `shroud-el--run'
;;; maybe need a fuzzer to check bogus arguments fed to the above
;;; The tests will depend on knowing the result, that is knowing the
;;; items in temp db and config
(ert-deftest shroud-test-el ()
  (let ((shroud-executable nil)
        (shroud-el--config-file shroud-test-config)
        (shroud-database-file shroud-test-db)
        (shroud-el--database-file shroud-test-db))
    (shroud--init)
    (should (equal (shroud-el--run shroud-test-db "list")
                   shroud-test--db))
    (should (equal (shroud-el--run shroud-test-db "list" shroud-test--e1)
                   (-take 1 shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "list" shroud-test--e1 shroud-test--e2)
                   (-take 2 shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "list" shroud-test--e1 shroud-test--e3)
                   (-remove-at 1 shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "list" shroud-test--e1 shroud-test--e1 shroud-test--e3 shroud-test--e3)
                   (-remove-at 1 shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "show" shroud-test--e1)
                   (car shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "show" shroud-test--e2)
                   (nth 1 shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "show" shroud-test--e3)
                   (nth 2 shroud-test--db)))
    (should (equal (shroud-el--run shroud-test-db "show" shroud-test--e1 "username")
                   '(("username" . "n00b"))))
    (should (equal (shroud-el--run shroud-test-db "show" shroud-test--e3 "username" "password")
                   '(("username" . "john-tho"))))
    (should (equal (and (null (shroud-el--run shroud-test-db "hide" shroud-test-e4)) (shroud-el--run shroud-test-db "list"))
                   (cons shroud-test-e4 shroud-test--db)))
    (should (equal (and (null (shroud-el--run shroud-test-db "hide" "--edit" shroud-test-e4)) (shroud-el--run shroud-test-db "list"))
                   (cons shroud-test-e4 shroud-test--db)))
    (should (equal (and (null (shroud-el--run shroud-test-db "remove" shroud-test--e4)) (shroud-el--run shroud-test-db "list"))
                   shroud-test--db))
    (should (equal (shroud-el--run shroud-test-db "list")
                   shroud-test--db))))

;;; Test `shroud-el-run' help strings
;;; help commands should output the familiar strings
(ert-deftest shroud-test-el-help ()
  (let ((shroud-executable nil)
        (shroud-database-file shroud-test-db)
        (shroud-el--config-file shroud-test-config)
        (shroud-el--database-file shroud-test-db))
    (shroud--init)
    (should (equal (shroud-el-run shroud-test-db "--help")
                   "Usage: shroud COMMAND ARGS. COMMAND may be one of:\nlist | show | remove | hide"))
    (should (equal (shroud-el-run shroud-test-db "list" "--help")
                   "Usage: shroud list [OPTION]\nShow the names of all secrets in the database."))
    (should (equal (shroud-el-run shroud-test-db "show" "--help")
                   "Usage: shroud show [OPTION] ID [KEY ...]\nShow secret named ID."))
    (should (equal (shroud-el-run shroud-test-db "remove" "--help")
                   "Usage: shroud remove [OPTION] id\nRemove a secret from the database."))
    (should (equal (shroud-el-run shroud-test-db "hide" "--help")
                   "Usage: shroud hide [OPTION] ID KEY=VALUE ...\nAdd a new secret named ID to the database."))
    (should (equal (shroud-el-run shroud-test-db "--version")
                   "shroud-el 1.12\nCopyright (C) 2019 Amar Singh\nGPLv3 or later"))))
;;; maybe need a fuzzer to check bogus arguments fed to the above

;;; Test `shroud-el-run' help strings
;;; The tests will depend on knowing the result, that is knowing the
;;; items in temp db and config
(ert-deftest shroud-test-el-ui ()
  (let ((shroud-executable nil)
        (shroud-database-file shroud-test-db)
        (shroud-el--config-file shroud-test-config)
        (shroud-el--database-file shroud-test-db))
    (shroud--init)
    (should (equal (shroud-el-run shroud-test-db "list")
                   "try1\n/dev/null\nbbc"))
    (should (equal (shroud-el-run shroud-test-db "list" shroud-test--e1)
                   "try1"))
    (should (equal (shroud-el-run shroud-test-db "list" shroud-test--e1 shroud-test--e2)
                   "try1\n/dev/null"))
    (should (equal (shroud-el-run shroud-test-db "list" shroud-test--e1 shroud-test--e1 shroud-test--e2 shroud-test--e2)
                   "try1\n/dev/null"))
    (should (equal (shroud-el-run shroud-test-db "show" shroud-test--e3)
                   "url news.com\nusername john-tho"))
    (should (equal (shroud-el-run shroud-test-db "show" shroud-test--e2)
                   "password edef4bd1dbf12c26d00ac8e50aac797f662e49bc\nemail leet@xyz.sh"))
    (should (equal (shroud-el-run shroud-test-db "show" shroud-test--e1 "username")
                   "n00b"))
    (should (equal (shroud-el-run shroud-test-db "show" shroud-test--e1 "username" "password")
                   "iCanHazCheez\nn00b"))
    ;; (should (equal (shroud-el-run shroud-test-db "hide" ) ;; todo hide is not working
    ;;                ))
    ;; (should (equal (shroud-el-run shroud-test-db "hide" "--edit" test)
    ;;                shroud-test--db))
    (should (equal (and (null (shroud-el-run shroud-test-db "remove" shroud-test--e4)) (null (shroud-el-run "show" shroud-test--e4)))
                   t))
    (should (equal (shroud-el-run shroud-test-db "list")
                   "try1\n/dev/null\nbbc"))))

(ert-deftest shroud-test-ui ()
  (let ((shroud-executable nil)
        (shroud-database-file shroud-test-db)
        (shroud-el--config-file shroud-test-config)
        (shroud-el--database-file shroud-test-db))
    (shroud--init)
    (should (equal (shroud--help) "Usage: shroud COMMAND ARGS. COMMAND may be one of:\nlist | show | remove | hide"))
    (should (equal (shroud--help--list) "Usage: shroud list [OPTION]\nShow the names of all secrets in the database."))
    (should (equal (shroud--help--remove)"Usage: shroud remove [OPTION] id\nRemove a secret from the database."))
    (should (equal (shroud--help--hide) "Usage: shroud hide [OPTION] ID KEY=VALUE ...\nAdd a new secret named ID to the database."))
    (should (equal (shroud--help--show) "Usage: shroud show [OPTION] ID [KEY ...]\nShow secret named ID."))
    (should (equal (shroud--version) "shroud-el 1.12\nCopyright (C) 2019 Amar Singh\nGPLv3 or later"))
    (should (equal (shroud--list) (list shroud-test--e1 shroud-test--e2 shroud-test--e3)))
;;    (should (equal (shroud--hide) val))
;;    (should (equal (shroud--hide-edit) val))
    (should (equal (shroud--show shroud-test--e1) "password iCanHazCheez\nusername n00b"))
    (should (equal (shroud--show-entry shroud-test--e3) '(("url" "news.com") ("username" "john-tho"))))
    (should (equal (shroud--show-sub-entries shroud-test--e2 "password") "edef4bd1dbf12c26d00ac8e50aac797f662e49bc"))
;;    (should (equal (shroud--show-clipboard) ))
    (should (equal (shroud--show-username shroud-test--e1) "n00b"))
    (should (equal (shroud--show-password shroud-test--e1) "iCanHazCheez"))
    (should (equal (shroud--show-url shroud-test--e3) "news.com"))
;;    (should (equal (shroud--remove) val))
;;    (should (equal (shroud--query shroud-test--e3) val))
    (should (equal (shroud--find "t") (list shroud-test--e1)))
    ))

;; Run all tests (ert t)

(provide 'shroud-test)

;;; shroud-test.el ends here
