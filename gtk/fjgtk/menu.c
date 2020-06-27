
#include <fju/fjgtk.h>

void fjgtk_add_menu_item( GtkWidget *hmenu, char *name, void (*cb)(), void *data ) {
  GtkWidget *item;

  item = (strcmp( name, "-" ) == 0) ? gtk_separator_menu_item_new() : gtk_menu_item_new_with_label( name );
  if( cb ) g_signal_connect( G_OBJECT(item), "activate", G_CALLBACK(cb), data );
  gtk_menu_shell_append( GTK_MENU_SHELL(hmenu), item );
  
}

GtkWidget *fjgtk_add_submenu( GtkWidget *hmenu, char *name ) {
  GtkWidget *submenu, *mitem;
  submenu = gtk_menu_new();
  mitem = gtk_menu_item_new_with_label( name );
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(mitem), submenu );
  gtk_menu_shell_append( GTK_MENU_SHELL(hmenu), mitem );
  return submenu;
}

