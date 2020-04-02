


#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <fju/fjgtk.h>

static void main_destroy( GtkWidget *wnd, gpointer data );
static void main_resize( GtkWidget *hwnd, gpointer data );
static void add_menubar( void );
static void entry_activate( GtkWidget *hwnd, gpointer data );
static void entry_changed( GtkEntry *hwnd, gpointer data );

int main( int argc, char **argv ) {
  GtkWidget *hmain, *fixed, *button, *hwnd;
  GtkWidget *menubar, *submenu, *mitem, *mitem2;
  
  /* always call this first */
  gtk_init( &argc, &argv );

  /* build main window */
  hmain = gtk_window_new( GTK_WINDOW_TOPLEVEL );
  fjgtk_register( hmain, "main", NULL );  
  g_signal_connect( hmain, "destroy", G_CALLBACK(main_destroy), NULL );
  g_signal_connect( hmain, "check-resize", G_CALLBACK(main_resize), NULL );
  
  gtk_window_set_default_size( GTK_WINDOW(hmain), 300, 200 );
  gtk_container_set_border_width( GTK_CONTAINER(hmain), 0 );
  
  /* add fixed container */
  fixed = gtk_fixed_new();
  fjgtk_register( fixed, "fixed", NULL );
  gtk_container_add( GTK_CONTAINER(hmain), fixed );

  /* add some buttons to the window */
  button = gtk_button_new_with_label( "Button1" );
  gtk_fixed_put( GTK_FIXED(fixed), button, 50, 50 );
  fjgtk_register( button, "button1", NULL );

  button = gtk_button_new_with_label( "Button2" );
  gtk_fixed_put( GTK_FIXED(fixed), button, 100, 50 );
  fjgtk_register( button, "button2", NULL );

  /* add menu */
  add_menubar();

  hwnd = gtk_label_new( "My little label" );
  gtk_fixed_put( GTK_FIXED(fixed), hwnd, 25, 30 );
  fjgtk_register( hwnd, "label", NULL );
  
  hwnd = gtk_entry_new();
  gtk_widget_set_size_request( hwnd, 100, 10 );
  gtk_fixed_put( GTK_FIXED(fixed), hwnd, 150, 25 );
  fjgtk_register( hwnd, "entry", NULL );
  
  g_signal_connect( G_OBJECT(hwnd), "activate", G_CALLBACK(entry_activate), NULL );
  g_signal_connect( G_OBJECT(hwnd), "changed", G_CALLBACK(entry_changed), NULL );
  
  gtk_widget_show_all( hmain );
  gtk_main();

  
  return 0;
}

static void main_destroy( GtkWidget *wnd, gpointer data ) {
  gtk_main_quit();
}

static void main_resize( GtkWidget *hmain, gpointer data ) {
  GtkWidget *hwnd;
  GtkFixed *fixed;
  gint w, h;

  gtk_window_get_size( GTK_WINDOW(hmain), &w, &h );

  fixed = (GtkFixed *)fjgtk_get( "fixed" );
  hwnd = fjgtk_get( "button1" );
  gtk_fixed_move( fixed, hwnd, w / 3, h / 3 );

  hwnd = fjgtk_get( "button1" );
  gtk_fixed_move( fixed, hwnd, (2 * w) / 3, (2 * h) / 3 );

}

static void add_menubar( void ) {
  GtkWidget *menubar, *submenu;
  
  /* add menu */
  menubar = gtk_menu_bar_new();
  fjgtk_register( menubar, "menubar", NULL );
  gtk_fixed_put( GTK_FIXED(fjgtk_get("fixed")), menubar, 0, 0 );
  
  submenu = fjgtk_add_submenu( menubar, "File" );
  fjgtk_register( submenu, "filemenu", NULL );

  submenu = fjgtk_add_submenu( submenu, "New" );
  fjgtk_add_menu_item( submenu, "x", NULL, NULL );
  fjgtk_add_menu_item( submenu, "y", NULL, NULL );
  
  submenu = fjgtk_get( "filemenu" );
  fjgtk_add_menu_item( submenu, "Open", NULL, NULL );
  fjgtk_add_menu_item( submenu, "-", NULL, NULL );
  fjgtk_add_menu_item( submenu, "Close", NULL, NULL );
  fjgtk_add_menu_item( submenu, "-", NULL, NULL );
  fjgtk_add_menu_item( submenu, "Quit", main_destroy, NULL );
  
  submenu = fjgtk_add_submenu( menubar, "Edit" );
  fjgtk_add_menu_item( submenu, "Copy", NULL, NULL );
  fjgtk_add_menu_item( submenu, "Cut", NULL, NULL );
  fjgtk_add_menu_item( submenu, "Paste", NULL, NULL );

}

static void entry_activate( GtkWidget *hwnd, gpointer data ) {
  printf( "entry activate\n" );
}

static void entry_changed( GtkEntry *hwnd, gpointer data ) {
  printf( "entry changed: %s\n", gtk_entry_get_text( hwnd ) );
}

