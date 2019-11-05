# Copyright (C) 2019 Amar Singh

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

outs = emacs-shroud.html emacs-shroud.txt emacs-shroud.texi emacs-shroud.info emacs-shroud

all: $(outs)

clean:
	-rm $(outs)

emacs-shroud.texi: README.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-texinfo-export-to-texinfo))"

emacs-shroud.info: emacs-shroud.texi
	makeinfo emacs-shroud.texi

emacs-shroud: emacs-shroud.texi
	makeinfo emacs-shroud.texi --html

emacs-shroud.html: emacs-shroud.texi
	makeinfo emacs-shroud.texi --html --no-split

emacs-shroud.txt: README.org
	makeinfo emacs-shroud.texi --plaintext > emacs-shroud.txt

emacs-shroud.pdf: README.org
	makeinfo emacs-shroud.texi --pdf
