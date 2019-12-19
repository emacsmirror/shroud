## Copyright (C) 2019  Amar M. Singh

## This file is part of Emacs-Shroud.

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

emacs_shroud_TEXINFOS = fdl-1.3.texi gpl-3.texi \
	urls.texi \
	prog.texi \
	version.texi

info_TEXINFOS  = emacs-shroud.texi

AM_MAKEINFOHTMLFLAGS = --css-include=extra/reset.css --css-include=extra/style.css --css-include=extra/manual.css --no-split

EXTRA_DIST = extra/manual.css \
	extra/reset.css \
	extra/style.css \
	extra/gendocs_template

all-local: info html

install-exec-local: install-html
