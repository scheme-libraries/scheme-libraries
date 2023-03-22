# Makefile for nathan.
# Copyright © Marc Nieper-Wißkirchen (2023).

scheme = scheme --libdirs lib
scheme-script = $(scheme) --program
prove = prove --exec '$(scheme-script)' --ext '.sps' --failures

all:

check:
	$(prove) tests

repl:
	$(scheme)

.PHONY: all check
