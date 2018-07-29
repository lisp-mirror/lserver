prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin

all:
	gcc -Wall -std=c99 interpreter.h interpreter.c lcli.c -o lcli -lreadline

install:
	mkdir -p "$(DESTDIR)$(bindir)"
	mv lcli "$(DESTDIR)$(bindir)"
