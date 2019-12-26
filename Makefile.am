# Copyright (C) 2019  Amar M. Singh

# This file is part of emacs-shroud.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

SUBDIRS = doc src
dist_doc_DATA = README

SUPPORT_EMAIL=emacs-shroud-users@nongnu.org

MANUAL_TITLE=Emacs-Shroud Reference Manual

custom_TEXINFOS  = doc/emacs-shroud.texi

AM_MAKEINFOHTMLFLAGS = --css-include=extra/reset.css --css-include=extra/style.css --css-include=extra/manual.css

.PHONY: manual
manual:
	$(MAKE) -C ./doc info
	GENDOCS_TEMPLATE_DIR=doc/extra ./scripts/gendocs.sh --email $(SUPPORT_EMAIL) -s $(custom_TEXINFOS) '$(MANUAL_TITLE)' -I ./doc/ --html '$(AM_MAKEINFOHTMLFLAGS)'

ChangeLog:
	git log --no-walk --tags --pretty="%h %d %s" --decorate=full > ChangeLog
