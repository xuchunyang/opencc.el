EMACS_ROOT = $(HOME)/Projects/emacs/
EMACS = $(EMACS_ROOT)/src/emacs

CC = gcc
LD = gcc
CFLAGS = -std=gnu99 -Wall -fPIC -I$(EMACS_ROOT)/src $(shell pkg-config --cflags opencc)
LDFLAGS = $(shell pkg-config --libs opencc)

all: opencc-core.so

opencc-core.so: opencc-core.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

opencc-core.o: opencc-core.c
	$(CC) $(CFLAGS) -c -o $@ $<

test:
	$(EMACS) -Q --batch -L . -l opencc-tests.el -f ert-run-tests-batch-and-exit

clean:
	-rm -f opencc-core.o opencc-core.so
