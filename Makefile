outs = emacs-shroud.html emacs-shroud.txt emacs-shroud.texi emacs-shroud.info emacs-shroud

all: $(outs)

clean:
	-rm $(outs)

emacs-shroud.texi: emacs-shroud.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-texinfo-export-to-texinfo))"

emacs-shroud.info: emacs-shroud.texi
	makeinfo emacs-shroud.texi

emacs-shroud: emacs-shroud.texi
	makeinfo emacs-shroud.texi --html

emacs-shroud.html: emacs-shroud.texi
	makeinfo emacs-shroud.texi --html --no-split

emacs-shroud.txt: emacs-shroud.org
	makeinfo emacs-shroud.texi --plaintext > emacs-shroud.txt

emacs-shroud.pdf: emacs-shroud.org
	makeinfo emacs-shroud.texi --pdf
