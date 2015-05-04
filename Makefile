RSCRIPTS = $(wildcard *[^_].r)

.PHONY: test

define \n


endef

test:
	$(foreach R,$(RSCRIPTS),Rscript $(R)$(\n))

print-%:
	@echo $* = $($*)
