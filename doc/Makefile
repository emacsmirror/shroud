outs=emacs-shroud.html emacs-shroud.info emacs-shroud emacs-shroud.txt dir

extra_outs=manual emacs-shroud.log emacs-shroud.aux emacs-shroud.tmp emacs-shroud.toc

default: emacs-shroud.info dir

all: $(outs)

clean:
	-rm -rf $(outs) $(extra_outs)

emacs-shroud.info: emacs-shroud.texi
	makeinfo emacs-shroud.texi

emacs-shroud: emacs-shroud.texi
	makeinfo emacs-shroud.texi --html --css-include=reset.css \
		--css-include=style.css \
		--css-include=manual.css

emacs-shroud.html: emacs-shroud.texi
	makeinfo emacs-shroud.texi --html --no-split --css-include=reset.css \
		--css-include=style.css \
		--css-include=manual.css

emacs-shroud.txt: emacs-shroud.texi
	makeinfo emacs-shroud.texi --plaintext > emacs-shroud.txt

emacs-shroud.pdf: emacs-shroud.texi
	makeinfo emacs-shroud.texi --pdf

www_manual: manual
	./gendocs.sh emacs-shroud 'Emacs-Shroud Reference Manual' --html '--css-include=reset.css --css-include=style.css --css-include=manual.css' --email 'emacs-shroud-users@nongnu.org'

manual: emacs-shroud.texi
	./gendocs.sh emacs-shroud 'Emacs-Shroud Reference Manual' --html '--css-include=reset.css --css-include=style.css --css-include=manual.css' --email 'emacs-shroud-users@nongnu.org'

dir: emacs-shroud.info
	install-info $< $@
