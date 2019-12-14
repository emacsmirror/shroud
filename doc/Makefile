outs = emacs-shroud.html emacs-shroud.info emacs-shroud emacs-shroud.txt # manual # emacs-shroud.pdf

all: $(outs)

clean:
	-rm -rf $(outs)

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

manual: emacs-shroud.texi
	./gendocs.sh emacs-shroud 'Emacs-Shroud Reference Manual' --html '--css-include=reset.css --css-include=style.css --css-include=manual.css' --email 'emacs-shroud-users@nongnu.org'
