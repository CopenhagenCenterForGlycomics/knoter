# Makefile for generating R packages, automatically updating the version
# number in the DESCRIPTION
#
# Roxygen uses the roxygen2 package, and will run automatically on check and all.

BUILDDIR = pkg

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

version_number:
	$(eval VERSION := $(shell ./tools/convertversion.sh))

$(BUILDDIR)/DESCRIPTION: DESCRIPTION version_number $(BUILDDIR) package_directories
	sed 's/^Version: .*$$/Version: '$(VERSION)'/' $< | sed 's/^Date: .*$$/Date: '`date "+%Y-%m-%d"`'/' > $@

DESCRIPTION-vars: $(BUILDDIR)/DESCRIPTION
	$(eval PKG_VERSION := $(shell grep -i ^version pkg/DESCRIPTION | cut -d : -d \  -f 2))
	$(eval PKG_NAME := $(shell grep -i ^package pkg/DESCRIPTION | cut -d : -d \  -f 2))

$(BUILDDIR)/R/%: R/%
	cp -f $< $@

$(BUILDDIR)/src/%: src/%
	cp -f $< $@

R_FILES := $(wildcard R/*.R)
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := $(BUILDDIR)/DESCRIPTION $(BUILDDIR)/NAMESPACE $(addprefix $(BUILDDIR)/,$(R_FILES)) $(addprefix $(BUILDDIR)/,$(SRC_FILES))
MAN_FILES := $(addprefix $(BUILDDIR)/,$(wildcard man/*))
TARBALL_NAME := $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(BUILDDIR)/man: man
	cp -f $</* $@

$(BUILDDIR)/NAMESPACE: NAMESPACE
	cp -f $< $@

package_directories:
	mkdir -p $(BUILDDIR)/R
	mkdir -p $(BUILDDIR)/src
	mkdir -p $(BUILDDIR)/man
 
.PHONY: tarball install check clean build DESCRIPTION-vars package_directories
 
$(PKG_NAME)_$(PKG_VERSION).tar.gz: package_directories $(PKG_FILES) package-documentation
	@echo $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD build $(BUILDDIR)

check: DESCRIPTION-vars $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
build: DESCRIPTION-vars $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
install: DESCRIPTION-vars $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE: $(R_FILES)
	Rscript -e "library(roxygen2);roxygenize('.',roclets=c('namespace'))"

.PHONY: package-documentation documentation
man documentation:
	Rscript -e "library(roxygen2);roxygenize('.',roclets=c('rd'))"	
package-documentation: man
	cp -f man/* $(BUILDDIR)/man/
#	Rscript -e "library(roxygen2);roxygenize('pkg',roclets=c('rd','namespace'))"


clean: DESCRIPTION-vars
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f man
	-rm -r -f $(BUILDDIR)


.SECONDEXPANSION:
tarball: DESCRIPTION-vars $$(TARBALL_NAME)

.PHONY: list
list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)