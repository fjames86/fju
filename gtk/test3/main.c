


#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

static void main_destroy( GtkWidget *wnd, gpointer data );
static void main_resize( GtkWidget *hwnd, gpointer data );

struct hentry {
  struct hentry *next;
  GtkWidget *hwnd;
  char *name;
};

static struct {
  struct hentry *handles;
} glob;

static GtkWidget *get_widget( char *name ) {
  struct hentry *h;
  h = glob.handles;
  while( h ) {
    if( strcmp( h->name, name ) == 0 ) return h->hwnd;
    h = h->next;
  }
  return NULL;
}
static void register_widget( GtkWidget *hwnd, char *name ) {
  struct hentry *h;
  h = malloc( sizeof(*h) );
  h->hwnd = hwnd;
  h->name = name;
  h->next = glob.handles;
  glob.handles = h;
}



int main( int argc, char **argv ) {
  GtkWidget *hmain, *fixed, *button;
  GtkWidget *menubar, *submenu, *mitem, *mitem2;
  
  /* always call this first */
  gtk_init( &argc, &argv );

  /* build main window */
  hmain = gtk_window_new( GTK_WINDOW_TOPLEVEL );
  register_widget( hmain, "main" );  
  g_signal_connect( hmain, "destroy", G_CALLBACK(main_destroy), NULL );
  g_signal_connect( hmain, "check-resize", G_CALLBACK(main_resize), NULL );
  
  gtk_window_set_default_size( GTK_WINDOW(hmain), 300, 200 );
  gtk_container_set_border_width( GTK_CONTAINER(hmain), 0 );
  
  /* add fixed container */
  fixed = gtk_fixed_new();
  register_widget( fixed, "fixed" );
  gtk_container_add( GTK_CONTAINER(hmain), fixed );

  /* add some buttons to the window */
  button = gtk_button_new_with_label( "Button1" );
  gtk_fixed_put( GTK_FIXED(fixed), button, 50, 50 );
  register_widget( button, "button1" );

  button = gtk_button_new_with_label( "Button2" );
  gtk_fixed_put( GTK_FIXED(fixed), button, 100, 50 );
  register_widget( button, "button2" );

  /* add menu */
  menubar = gtk_menu_bar_new();
  register_widget( menubar, "menubar" );
  gtk_fixed_put( GTK_FIXED(fixed), menubar, 0, 0 );
  
  submenu = gtk_menu_new();
  mitem = gtk_menu_item_new_with_label( "File" );
  mitem2 = gtk_menu_item_new_with_label( "Quit" );
  g_signal_connect( G_OBJECT(mitem2), "activate", G_CALLBACK(main_destroy), NULL );
  
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(mitem), submenu );
  gtk_menu_shell_append( GTK_MENU_SHELL(submenu), mitem2 );
  gtk_menu_shell_append( GTK_MENU_SHELL(menubar), mitem );
  
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

  fixed = (GtkFixed *)get_widget( "fixed" );
  hwnd = get_widget( "button1" );
  gtk_fixed_move( fixed, hwnd, w / 3, h / 3 );

  hwnd = get_widget( "button1" );
  gtk_fixed_move( fixed, hwnd, (2 * w) / 3, (2 * h) / 3 );

}

