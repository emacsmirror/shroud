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

AC_INIT([emacs-shroud],
	[1.105],
	[emacs-shroud-users@gnu.org], [emacs-shroud],
	[https://www.nongnu.org/emacs-shroud/])

AC_CONFIG_SRCDIR([src])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AM_SILENT_RULES([yes])

AM_INIT_AUTOMAKE([-Wall -Werror])

AC_CONFIG_FILES([
Makefile
src/Makefile
doc/Makefile
])

AM_PATH_LISPDIR

dnl subst emacs
AC_PATH_PROG([EMACS], [emacs])

AC_OUTPUT
