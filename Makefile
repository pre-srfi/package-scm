.PHONY : all \
	clean clean-deps clean-cache \
	install-deps browse-html

all : packhack.html

clean : clean-deps clean-cache
	rm -f packhack.html

clean-deps :
	-raco pkg remove --demote sxml
	-raco pkg remove --demote css-expr
	-raco pkg remove --demote html-parsing
	-raco pkg remove --demote txexpr

clean-cache :
	rm -rf .cache/

# raco install in Makefiles: https://github.com/search?l=Makefile&p=3&q=%22raco+pkg+install%22+makefile&type=Code
# or (install or update): https://stackoverflow.com/a/51125833/2512585
install-deps :
	-raco pkg install --skip-installed --deps search-auto \
	    css-expr html-parsing sxml txexpr

# Note: see http://lassi.io/temp/packhack.html for a complete result page
packhack.html : install-deps
	racket packhack.rkt

# Note: this obviously assumes firefox to be installed; adapt as required
browse-html : packhack.html
	firefox $<
