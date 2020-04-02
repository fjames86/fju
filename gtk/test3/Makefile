
SOURCES=main.c resources.c 

rpc: $(SOURCES)
	gcc `pkg-config --cflags gtk+-3.0` -o test -I../../include -L../fjgtk -g $(SOURCES) `pkg-config --libs gtk+-3.0` -export-dynamic -lfjgtk

resources.c: resources.xml 
	glib-compile-resources resources.xml --target=resources.c --generate-source

