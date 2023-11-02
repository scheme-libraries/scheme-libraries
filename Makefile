# Makefile for scheme libraries.
# Copyright © Marc Nieper-Wißkirchen (2023).

libdirs = lib:x86_64-linux-gnu/lib
scheme = scheme --libdirs $(libdirs)
scheme-script = $(scheme) --program
compile = $(scheme-script) compile.sps
prove = prove --exec '$(scheme-script)' --ext '.sps' --failures

all:

check:
	$(prove) tests/test-*.sps

repl:
	@$(scheme) schemerc

clean:
	find . -type f -name '*.so' -exec rm {} \;
	find . -type f -name '*.wpo' -exec rm {} \;
	rm -f tests/test-eval

compile:
	$(compile) tests/test-eval

.PHONY: all check repl
