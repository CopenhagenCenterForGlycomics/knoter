# Makefile for generating R packages, automatically updating the version
# number in the DESCRIPTION
#
# Roxygen uses the roxygen2 package, and will run automatically on check and all.

version_number:
	$(eval VERSION := $(shell ./tools/convertversion.sh))

DESCRIPTION: src/DESCRIPTION version_number
	sed 's/^Version: .*$$/Version: '$(VERSION)'/' $< | sed 's/^Date: .*$$/Date: '`date "+%Y-%m-%d"`'/' > $@

DESCRIPTION-vars: DESCRIPTION
	$(eval PKG_VERSION := $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2))
	$(eval PKG_NAME := $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2))

R_FILES := $(wildcard R/*.R)
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)
TARBALL_NAME := $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
.PHONY: tarball install check clean build DESCRIPTION-vars
 
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	@echo $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD build .

check: DESCRIPTION-vars $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
build: DESCRIPTION-vars $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
install: DESCRIPTION-vars $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE: $(R_FILES)
	Rscript -e "library(roxygen2);roxygenize('.')"

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f man/*
	-rm -r -f NAMESPACE

.SECONDEXPANSION:
tarball: DESCRIPTION-vars $$(TARBALL_NAME)

.PHONY: list
list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)