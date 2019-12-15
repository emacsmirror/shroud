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

SUBDIRS = doc

.ONESHELL:
all:
	emacs -Q --batch --script ./make.el
	$(MAKE) -C ./doc

.PHONY: clean

clean:
	-rm -rf bin
	-rm *.elc
	$(MAKE) -C ./doc clean

check: test

test: shroud-test.el
	emacs -Q -batch -l ert \
	-l shroud-el.el shroud-cli.el shroud-bui.el shroud.el \
	-l $< -f ert-run-tests-batch-and-exit
