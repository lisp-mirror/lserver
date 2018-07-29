prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin

all: server client

.PHONY: client server

server:
	cd server; make

client:
	cd client; make

install_server:
	cd server; make install

install_client:
	cd client; make install

install: install_server install_client

