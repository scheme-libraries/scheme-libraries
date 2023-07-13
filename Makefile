# Makefile for scheme libraries.
# Copyright © Marc Nieper-Wißkirchen (2023).

libdirs = lib:x86_64-linux-gnu/lib
scheme = scheme --libdirs $(libdirs)
scheme-script = $(scheme) --program
prove = prove --exec '$(scheme-script)' --ext '.sps' --failures

all:

check:
	$(prove) tests

repl:
	@$(scheme) schemerc

.PHONY: all check repl
