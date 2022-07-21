
# Package name and version number:
dist = scale-$(version)
version = 0.4

# Default installation prefix.
prefix = /usr/local

bindir = $(prefix)/bin
datadir = $(prefix)/share/scale

DISTFILES = COPYING Makefile README.md scale.html scale.png scale.lua \
lib/* scl/*.scl

all:

clean:
	rm -f scale *~

# documentation (this needs pandoc)

# I don't actually use this, instead I render the document using Typora with
# its Github theme; this looks much nicer. But this gives you a quick way to
# regenerate the documentation after changes if you have pandoc installed.

PDF_FLAGS = -V "geometry:paperwidth=21cm" -V "geometry:paperheight=29.7cm" \
-V "geometry:vmargin=2cm" -V "geometry:hmargin=2cm" -V "fontsize:12pt"
#PDF_FLAGS += -V "fontfamily:mathpazo" -V "mainfont:Palatino Linotype"
#PDF_FLAGS += -V "mainfont:Palatino Linotype"
#PDF_FLAGS += -V "mainfont:Arial"
PDF_FLAGS += --pdf-engine=xelatex -V colorlinks=true

html: scale.html
pdf: scale.pdf

scale.html: README.md
	pandoc $< -o $@

scale.pdf: README.md
	pandoc $< -o $@ $(PDF_FLAGS)

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	ln -sf "$(datadir)/scale.lua" "$(DESTDIR)$(bindir)/scale"
	test -d "$(DESTDIR)$(datadir)" || mkdir -p "$(DESTDIR)$(datadir)"
	cp -R -p scale.lua scale.html scale.png lib scl "$(DESTDIR)$(datadir)"

uninstall:
	rm -rf "$(DESTDIR)$(bindir)/scale" "$(DESTDIR)$(datadir)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/lib && mkdir $(dist)/scl
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
