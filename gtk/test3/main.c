


#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

static void main_destroy( GtkWidget *wnd, gpointer data );
static void main_resize( GtkWidget *hwnd, gpointer data );
static void add_menubar( void );

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
  add_menubar();

  
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


static void add_menu_item( GtkWidget *hmenu, char *name, void (*cb)(), void *data ) {
  GtkWidget *item;

  item = name == "-" ? gtk_separator_menu_item_new() : gtk_menu_item_new_with_label( name );
  if( cb ) g_signal_connect( G_OBJECT(item), "activate", G_CALLBACK(cb), data );
  gtk_menu_shell_append( GTK_MENU_SHELL(hmenu), item );
  
}

static GtkWidget *add_submenu( GtkWidget *hmenu, char *name ) {
  GtkWidget *submenu, *mitem;
  submenu = gtk_menu_new();
  mitem = gtk_menu_item_new_with_label( name );
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(mitem), submenu );
  gtk_menu_shell_append( GTK_MENU_SHELL(hmenu), mitem );
  return submenu;
}

static void add_menubar( void ) {
  GtkWidget *menubar, *submenu;
  
  /* add menu */
  menubar = gtk_menu_bar_new();
  register_widget( menubar, "menubar" );
  gtk_fixed_put( GTK_FIXED(get_widget("fixed")), menubar, 0, 0 );
  
  submenu = add_submenu( menubar, "File" );
  register_widget( submenu, "filemenu" );

  submenu = add_submenu( submenu, "New" );
  add_menu_item( submenu, "x", NULL, NULL );
  add_menu_item( submenu, "y", NULL, NULL );
  
  submenu = get_widget( "filemenu" );
  add_menu_item( submenu, "Open", NULL, NULL );
  add_menu_item( submenu, "-", NULL, NULL );
  add_menu_item( submenu, "Close", NULL, NULL );
  add_menu_item( submenu, "-", NULL, NULL );
  add_menu_item( submenu, "Quit", main_destroy, NULL );
  
  submenu = add_submenu( menubar, "Edit" );
  add_menu_item( submenu, "Copy", NULL, NULL );
  add_menu_item( submenu, "Cut", NULL, NULL );
  add_menu_item( submenu, "Paste", NULL, NULL );

}
