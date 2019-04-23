.PHONY : all \
	clean clean-deps clean-cache \
	install-deps package-htmls browse-html

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
	-raco pkg install --skip-installed sxml
	-raco pkg install --skip-installed css-expr
	-raco pkg install --skip-installed html-parsing
	-raco pkg install --skip-installed --deps search-auto txexpr

.PHONY : -html-cache
-html-cache : .cache/

.cache/ :
	mkdir -p $@

package-htmls : -html-cache \
	.cache/akku-index.scm .cache/egg-index-4.html .cache/egg-index-5.html .cache/gauche-packages.html .cache/guile-packages.html

# Note: see http://lassi.io/temp/packhack.html for a complete result page
packhack.html : install-deps package-htmls
	racket packhack.rkt

# Note: this obviously assumes firefox to be installed; adapt as required
browse-html : packhack.html
	firefox $<

.cache/akku-index.scm :
	wget -O $@ https://akkuscm.org/packages/

.cache/egg-index-4.html :
	wget -O $@ http://eggs.call-cc.org/4/

.cache/egg-index-5.html :
	wget -O $@ http://eggs.call-cc.org/5/

.cache/gauche-packages.html :
	wget -O $@ http://practical-scheme.net/wiliki/wiliki.cgi/Gauche:Packages

.cache/guile-packages.html :
	wget -O $@ https://www.gnu.org/software/guile/libraries/

.cache/ravensc-readme.md :
	wget --header="Accept: text/plain" -O $@ https://raw.githubusercontent.com/guenchi/Raven/master/README.md

.cache/ravensc-packages.json :
	wget --post-data="" --header="Accept: application/json" -O $@ http://ravensc.com/list
