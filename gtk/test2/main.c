


#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

static void main_destroy( GtkWidget *wnd, gpointer data );
static void test_button_click( GtkWidget *hwnd, gpointer data );
static void add_buttons( GtkWidget *hgrid );

int main( int argc, char **argv ) {
  GtkWidget *win;
  GtkWidget *hwnd, *hgrid;
  GValue gval = G_VALUE_INIT;
  GtkWidget *center_vbox, *menubar, *menuItem1, *submenu1, *item1, *item2, *combobox;
  GtkListStore *combols;
  GtkCellRenderer *renderer;
  GtkTreeIter iter;
  
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
  gtk_grid_insert_row( GTK_GRID(hgrid), 1 );
  gtk_grid_set_column_spacing( GTK_GRID(hgrid), 10 );
  gtk_grid_set_column_homogeneous( GTK_GRID(hgrid), TRUE );
  gtk_widget_set_halign( hgrid, GTK_ALIGN_START );
  gtk_container_add( GTK_CONTAINER(win), hgrid );
  
  /* add some buttons */
  add_buttons( hgrid );

  /* add menu */
  center_vbox = gtk_box_new( GTK_ORIENTATION_VERTICAL, 0 );
  menubar = gtk_menu_bar_new();
  menuItem1 = gtk_menu_item_new_with_mnemonic( "_Application" );
  submenu1 = gtk_menu_new();
  item1 = gtk_menu_item_new_with_label( "Item1" );
  item2 = gtk_menu_item_new_with_label( "Quit" );

  combols = gtk_list_store_new( 1, G_TYPE_STRING );
  gtk_list_store_append( combols, &iter );
  gtk_list_store_set( combols, &iter, 0, "Choose 1", -1 );
  gtk_list_store_append( combols, &iter );
  gtk_list_store_set( combols, &iter, 0, "Choose 2", -1 );
  gtk_list_store_append( combols, &iter );
  gtk_list_store_set( combols, &iter, 0, "Choose 3", -1 );
  gtk_list_store_append( combols, &iter );
  combobox = gtk_combo_box_new_with_model( GTK_TREE_MODEL(combols) );
  g_object_unref( combols );

  renderer = gtk_cell_renderer_text_new();
  gtk_cell_layout_pack_start( GTK_CELL_LAYOUT(combobox), renderer, TRUE );
  gtk_cell_layout_set_attributes( GTK_CELL_LAYOUT(combobox), renderer, "text", 0, NULL );
  gtk_combo_box_set_active( GTK_COMBO_BOX(combobox), 0 );
  gtk_menu_shell_append( GTK_MENU_SHELL(submenu1), item1 );
  gtk_menu_shell_append( GTK_MENU_SHELL(submenu1), item2 );
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(menuItem1), submenu1 );
  gtk_menu_shell_append( GTK_MENU_SHELL(menubar), menuItem1 );
  gtk_box_pack_start( GTK_BOX(center_vbox), menubar, TRUE, TRUE, 0 );
  gtk_box_pack_start( GTK_BOX(center_vbox), combobox, FALSE, FALSE, 0 );
  //  gtk_container_add( GTK_CONTAINER(win), center_vbox );
  gtk_grid_attach( GTK_GRID(hgrid), center_vbox, 0, 0, 2, 1 );
  
  gtk_widget_show_all( win );
  gtk_main();

  /* close libraries */
  
  return 0;
}

static void main_destroy( GtkWidget *wnd, gpointer data ) {
  gtk_main_quit();
}

static void test_dialog_click( GtkDialog *hdlg, gint id, gpointer data ) {
  printf( "Dialog clicked %d\n", id );
  gtk_widget_destroy( GTK_WIDGET(hdlg) );
}

static void test_button_click( GtkWidget *hwnd, gpointer data ) {
  GtkWidget *hdlg;
  
  printf( "click\n" );

  hdlg = gtk_message_dialog_new( GTK_WINDOW(gtk_widget_get_toplevel( hwnd )), GTK_DIALOG_MODAL, GTK_MESSAGE_INFO, GTK_BUTTONS_OK_CANCEL, "message" );
  //  gtk_dialog_add_buttons( GTK_DIALOG(hdlg), "OK", 12, "Cancell?", 13, NULL );
  g_signal_connect( GTK_DIALOG(hdlg), "response", G_CALLBACK(test_dialog_click), NULL );
  gtk_dialog_run( GTK_DIALOG(hdlg) );

}

static void add_buttons( GtkWidget *hgrid ) {
  GtkWidget *hwnd;
  
  /* add some buttons */
  hwnd = gtk_button_new_with_label( "A test button 1" );
  g_signal_connect( hwnd, "clicked", G_CALLBACK(test_button_click), NULL );
  gtk_widget_set_size_request( hwnd, 70, 30 );
  gtk_widget_set_halign( hwnd, GTK_ALIGN_START );
  gtk_grid_attach( GTK_GRID(hgrid), hwnd, 0, 1, 1, 1 );

  hwnd = gtk_button_new_with_label( "A test button 2" );
  g_signal_connect( hwnd, "clicked", G_CALLBACK(test_button_click), NULL );
  gtk_widget_set_size_request( hwnd, 70, 30 );
  gtk_widget_set_halign( hwnd, GTK_ALIGN_START );
  gtk_grid_attach( GTK_GRID(hgrid), hwnd, 1, 1, 1, 1 );

}

static void add_menu( GtkWidget *win ) {
}
