outs=emacs-shroud.html emacs-shroud.info emacs-shroud emacs-shroud.txt dir

extra_outs=manual emacs-shroud.log emacs-shroud.aux emacs-shroud.tmp emacs-shroud.toc

texi_source=emacs-shroud.texi

html_args=--css-include=reset.css \
		--css-include=style.css \
		--css-include=manual.css

support_email='emacs-shroud-users@nongnu.org'

default: emacs-shroud.info dir

all: $(outs)

clean:
	-rm -rf $(outs) $(extra_outs)

emacs-shroud.info: $(texi_source)
	makeinfo $(texi_source)

emacs-shroud: $(texi_source)
	makeinfo $(texi_source) --html $(html_args)

emacs-shroud.html: $(texi_source)
	makeinfo $(texi_source) --html $(html_args) --no-split

emacs-shroud.txt: $(texi_source)
	makeinfo $(texi_source) --plaintext > emacs-shroud.txt

emacs-shroud.pdf: $(texi_source)
	makeinfo $(texi_source) --pdf

www_manual: manual

manual: $(texi_source)
	./gendocs.sh emacs-shroud 'Emacs-Shroud Reference Manual' --html '$(html_args)' --email '$(support_email)'

dir: emacs-shroud.info
	install-info $< $@
