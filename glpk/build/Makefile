GLPK_VERSION=5.0

.PHONY: clean make-glpk

glpk-$(GLPK_VERSION).tar.gz:
	wget https://ftpmirror.gnu.org/gnu/glpk/$@

glpk-$(GLPK_VERSION): glpk-$(GLPK_VERSION).tar.gz
	tar -xvzf $<
	cd

glpsol-$(GLPK_VERSION): glpk-$(GLPK_VERSION)
	$(MAKE) -C $< -f $(CURDIR)/Makefile make-glpk
	cp $</examples/.libs/glpsol $@

clean:
	-@rm -rf glpk-*
	-@rm glpsol*

# This sub-section is intended to be ran from glpsol-XY folder
make-glpk:
	./configure
	$(MAKE)
