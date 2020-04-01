


#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

static void main_destroy( GtkWidget *wnd, gpointer data );
static void test_button_click( GtkWidget *hwnd, gpointer data );

int main( int argc, char **argv ) {
  GtkWidget *win;
  GtkWidget *hwnd, *hgrid;
  GValue gval = G_VALUE_INIT;
  
  /* always call this first */
  gtk_init( &argc, &argv );

  /* build main window */
  win = gtk_window_new( GTK_WINDOW_TOPLEVEL );
  g_signal_connect( win, "destroy", G_CALLBACK(main_destroy), NULL );
  gtk_window_set_default_size( GTK_WINDOW(win), 300, 200 );
  gtk_container_set_border_width( GTK_CONTAINER(win), 15 );

  /* allocate a grid and put some buttons on it */
  hgrid = gtk_grid_new();
  gtk_grid_insert_column( GTK_GRID(hgrid), 0 );
  gtk_grid_insert_column( GTK_GRID(hgrid), 1 );
  gtk_grid_insert_row( GTK_GRID(hgrid), 0 );
  gtk_grid_set_column_spacing( GTK_GRID(hgrid), 10 );
  gtk_grid_set_column_homogeneous( GTK_GRID(hgrid), TRUE );
  gtk_widget_set_halign( hgrid, GTK_ALIGN_START );

  /* add some buttons */
  hwnd = gtk_button_new_with_label( "A test button 1" );
  g_signal_connect( hwnd, "clicked", G_CALLBACK(test_button_click), NULL );
  gtk_widget_set_size_request( hwnd, 70, 30 );
  gtk_widget_set_halign( hwnd, GTK_ALIGN_START );
  gtk_grid_attach( GTK_GRID(hgrid), hwnd, 0, 0, 1, 1 );

  hwnd = gtk_button_new_with_label( "A test button 2" );
  g_signal_connect( hwnd, "clicked", G_CALLBACK(test_button_click), NULL );
  gtk_widget_set_size_request( hwnd, 70, 30 );
  gtk_widget_set_halign( hwnd, GTK_ALIGN_START );
  gtk_grid_attach( GTK_GRID(hgrid), hwnd, 1, 0, 1, 1 );
  
  gtk_container_add( GTK_CONTAINER(win), hgrid );

  gtk_widget_show_all( win );
  gtk_main();

  /* close libraries */
  
  return 0;
}

static void main_destroy( GtkWidget *wnd, gpointer data ) {
  gtk_main_quit();
}

static void test_button_click( GtkWidget *hwnd, gpointer data ) {
  printf( "click\n" );
}

