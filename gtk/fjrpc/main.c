


#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

#include "fju.h"

/* globals */
static struct {
  GtkBuilder *builder[8];
#define B_MAIN 0
  GtkWindow *wndmain;
} globs;

static gint fjrpc_timeout_cb( gpointer data );

int main( int argc, char **argv ) {
  GtkBuilder *builder;
  GtkWidget *win;

  /* always call this first */
  gtk_init( &argc, &argv );

  /* build main window */
  builder = gtk_builder_new();
  gtk_builder_add_from_resource( builder, "/com/fju/fjrpc/main.glade", NULL );
  globs.builder[B_MAIN] = builder;
  
  /* get main */
  win = GTK_WIDGET( gtk_builder_get_object( builder, "main" ) );
  globs.wndmain = GTK_WINDOW( win );

  /* connect callback funcitons */
  gtk_builder_connect_signals( builder, NULL );

  /* open libraries, initialise gui etc */

  /* set timer callback so we can do things periodically */
  g_timeout_add( 5000, fjrpc_timeout_cb, NULL );
  
  gtk_widget_show( win );
  gtk_main();

  /* close libraries */
  
  return 0;
}

void fjrpc_window_main_destroy( void ) {
  gtk_main_quit();
}

static gint fjrpc_timeout_cb( gpointer data ) {
  printf( "Timer\n" );
  return 1;
}

GtkWidget *fjrpc_widget( int bidx, char *name ) {
  return GTK_WIDGET( gtk_builder_get_object( globs.builder[bidx], name ) );
}

void fjrpc_button1_clicked( GtkButton *wnd, gpointer data ) {
  int sts;
  sts = fju_messagebox_yesno( globs.wndmain, "Really quit?");
  if( sts == GTK_RESPONSE_YES ) {
    gtk_main_quit();
  }
  
}

void fjrpc_button2_clicked( GtkButton *wnd, gpointer data ) {
  int sts;
 
  sts = fju_messagebox_yesno( globs.wndmain, "Hello. Please click yes or no. Current time %d", (int)time( NULL ) );
  fju_messagebox_error( globs.wndmain, "You clicked %d", sts );
  
}
