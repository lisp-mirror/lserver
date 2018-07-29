prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin

all:
	sbcl --load make-image.lisp

install:
	mkdir -p "$(DESTDIR)$(bindir)"
	mv lserver "$(DESTDIR)$(bindir)"
