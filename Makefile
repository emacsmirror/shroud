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

inputs=shroud-el.el shroud-cli.el shroud-bui.el shroud.el

tests=shroud-test.el

tag_old=1.83.4

tag=1.83.24

dists=emacs-shroud-$(tag).tar.gz emacs-shroud-latest.tar.gz emacs-shroud-$(tag).zip

.ONESHELL:
all:
	emacs -Q --batch --script ./make.el
	$(MAKE) -C ./doc

.PHONY: clean

clean:
	-rm -rf bin
	-rm *.elc
	-rm $(dists)
	$(MAKE) -C ./doc clean

check: test

test: $(tests)
	emacs -Q -batch -l ert \
	-l shroud-el.el shroud-cli.el shroud-bui.el shroud.el \
	-l $< -f ert-run-tests-batch-and-exit

dist-release: $(inputs) $(tests) changelog
	git archive --format tar.gz $(tag) -6 -o emacs-shroud-$(tag).tar.gz

dist-latest: $(inputs) $(tests)
	git archive --format tar.gz HEAD -6 -o emacs-shroud-latest.tar.gz

dist-zip: $(inputs) $(tests)
	git archive --format zip $(tag) -6 -o emacs-shroud-$(tag).zip

changelog:
	git log $(tag_old)...$(tag) --pretty > changelog
