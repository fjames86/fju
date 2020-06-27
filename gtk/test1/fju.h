
#ifndef FJU_H
#define FJU_H

#include <gtk/gtk.h>

int fju_messagebox_error( GtkWindow *parent, char *fmt, ... );
int fju_messagebox_okcancel( GtkWindow *parent, char *fmt, ... );
int fju_messagebox_yesno( GtkWindow *parent, char *fmt, ... );

GtkWidget *fjrpc_widget( int bidx, char *name );

#endif
