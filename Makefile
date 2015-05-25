# Makefile for generating R packages.
# 2011 Andrew Redd
#
# Roxygen uses the roxygen2 package, and will run automatically on check and all.

version_number:
	$(eval VERSION := $(shell ./tools/convertversion.sh))

DESCRIPTION: src/DESCRIPTION version_number
	@echo $<
	@echo $(VERSION)
	sed 's/^Version: .*$$/Version: '$(VERSION)'/' $< | sed 's/^Date: .*$$/Date: '`date "+%Y-%m-%d"`'/' > $@

PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
 
R_FILES := $(wildcard R/*.R)
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)
 
.PHONY: tarball install check clean build
 
tarball: $(PKG_NAME)_$(PKG_VERSION).tar.gz
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build .

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE: $(R_FILES)
	Rscript -e "library(roxygen2);roxygenize('pkg')"

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f man/*
	-rm -r -f NAMESPACE
.PHONY: list
list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)