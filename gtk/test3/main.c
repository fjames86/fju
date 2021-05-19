


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
static void add_treeview( void );
static void button1_click( GtkWidget *hwnd, gpointer data );
static void button2_click( GtkWidget *hwnd, gpointer data );

int main( int argc, char **argv ) {
  GtkWidget *hmain, *fixed, *button, *hwnd;
  GtkWidget *menubar, *submenu, *mitem, *mitem2;
  
  /* always call this first */
  gtk_init( &argc, &argv );

  /* build main window */
  hmain = gtk_window_new( GTK_WINDOW_TOPLEVEL );
  fjgtk_register( hmain, "main" );  
  g_signal_connect( hmain, "destroy", G_CALLBACK(main_destroy), NULL );
  g_signal_connect( hmain, "check-resize", G_CALLBACK(main_resize), NULL );
  
  gtk_window_set_default_size( GTK_WINDOW(hmain), 400, 200 );
  gtk_container_set_border_width( GTK_CONTAINER(hmain), 0 );
  
  /* add fixed container */
  fixed = gtk_fixed_new();
  fjgtk_register( fixed, "fixed" );
  gtk_container_add( GTK_CONTAINER(hmain), fixed );

  /* add some buttons to the window */
  button = gtk_button_new_with_label( "Add item" );
  gtk_fixed_put( GTK_FIXED(fixed), button, 150, 65 );
  fjgtk_register( button, "button1" );
  g_signal_connect( G_OBJECT(button), "clicked", G_CALLBACK(button1_click), NULL );
  
  button = gtk_button_new_with_label( "Clear items" );
  gtk_fixed_put( GTK_FIXED(fixed), button, 150, 100 );
  fjgtk_register( button, "button2" );
  g_signal_connect( G_OBJECT(button), "clicked", G_CALLBACK(button2_click), NULL );
  
  /* add menu */
  add_menubar();

  /* add a label and text box */
  hwnd = gtk_label_new( "My little label" );
  gtk_fixed_put( GTK_FIXED(fixed), hwnd, 25, 30 );
  fjgtk_register( hwnd, "label" );
  
  hwnd = gtk_entry_new();
  gtk_widget_set_size_request( hwnd, 100, 10 );
  gtk_fixed_put( GTK_FIXED(fixed), hwnd, 150, 25 );
  fjgtk_register( hwnd, "entry" );  
  g_signal_connect( G_OBJECT(hwnd), "activate", G_CALLBACK(entry_activate), NULL );
  g_signal_connect( G_OBJECT(hwnd), "changed", G_CALLBACK(entry_changed), NULL );

  /* add tree view */
  add_treeview();
  

  
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
  gtk_fixed_move( fixed, fjgtk_get( "button1" ), w / 2, 150 );
  gtk_fixed_move( fixed, fjgtk_get( "button2" ), w / 2, 100 );

}

static void add_menubar( void ) {
  GtkWidget *menubar, *submenu;
  
  /* add menu */
  menubar = gtk_menu_bar_new();
  fjgtk_register( menubar, "menubar" );
  gtk_fixed_put( GTK_FIXED(fjgtk_get("fixed")), menubar, 0, 0 );
  
  submenu = fjgtk_add_submenu( menubar, "File" );
  fjgtk_register( submenu, "filemenu" );

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

}

static void add_listview_item( GtkWidget *listview, char *itemtext ) {
  GtkListStore *store;
  GtkTreeIter iter;
  
  store = GTK_LIST_STORE(gtk_tree_view_get_model( GTK_TREE_VIEW(listview) ));
  gtk_list_store_append( store, &iter );
  gtk_list_store_set( store, &iter, 0, itemtext, -1 );
}

static void add_treeview( void ) {
  GtkWidget *list;
  GtkCellRenderer *renderer;
  GtkListStore *store;
  GtkTreeViewColumn *column;
  
  list = gtk_tree_view_new();
  fjgtk_register( list, "treelist" );
  gtk_tree_view_set_headers_visible( GTK_TREE_VIEW(list), TRUE );
  gtk_fixed_put( GTK_FIXED(fjgtk_get( "fixed" )), list, 25, 100 );
  gtk_widget_set_size_request( list, 50, 100 );
  
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes( "List items", GTK_CELL_RENDERER(renderer), "text", 0, NULL );
  gtk_tree_view_append_column( GTK_TREE_VIEW(list), GTK_TREE_VIEW_COLUMN(column) );
  store = gtk_list_store_new( 1, G_TYPE_STRING );
  gtk_tree_view_set_model( GTK_TREE_VIEW(list), GTK_TREE_MODEL(store) );
  g_object_unref( store );

  add_listview_item( list, "item 1" );
  add_listview_item( list, "item 2" );
  add_listview_item( list, "item 3" );
  
}

static void button1_click( GtkWidget *hwnd, gpointer data ) {
  char *str;
  str = (char *)gtk_entry_get_text( GTK_ENTRY(fjgtk_get( "entry" )) );
  add_listview_item( fjgtk_get( "treelist" ), str );
}

static void button2_click( GtkWidget *hwnd, gpointer data ) {
  GtkListStore *store;
  store = GTK_LIST_STORE(gtk_tree_view_get_model( GTK_TREE_VIEW(fjgtk_get( "treelist" )) ));
  gtk_list_store_clear( store );
}

