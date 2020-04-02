
#ifndef FJGTK_H
#define FJGTK_H

#include <gtk/gtk.h>

int fjgtk_register( GtkWidget *hwnd, char *name );
GtkWidget *fjgtk_get( char *name );
GtkWidget *fjgtk_get_by_id( int id );
void fjgtk_add_menu_item( GtkWidget *hmenu, char *name, void (*cb)(), void *data );
GtkWidget *fjgtk_add_submenu( GtkWidget *hmenu, char *name );

#endif

