.PHONY: all
all: doc vignettes test

R = Rscript --no-save --no-restore -e

test:
	$(R) "devtools::test()"

rmd_files=$(wildcard vignettes/*.rmd)
knit_results=$(patsubst vignettes/%.rmd,inst/doc/%.md,$(rmd_files))

inst/doc:
	mkdir -p $@

inst/doc/%.md: vignettes/%.rmd
	$(R) "knitr::knit('$<', '$@')"

.PHONY: rcpp
rcpp:
	$(R) "Rcpp::compileAttributes()"

.PHONY: vignettes
vignettes: inst/doc ${knit_results}
	$(R) "library(knitr); library(devtools); build_vignettes()"

.PHONY: doc
doc:
	$(R) "devtools::document()"

.PHONY: clean
clean:
	${RM} -r inst/doc
	${RM} -r man
