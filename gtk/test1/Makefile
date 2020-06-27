
SOURCES=main.c resources.c utils.c

rpc: $(SOURCES)
	gcc `pkg-config --cflags gtk+-3.0` -o test -g $(SOURCES) `pkg-config --libs gtk+-3.0` -export-dynamic

resources.c: resources.xml main.glade 
	glib-compile-resources resources.xml --target=resources.c --generate-source

