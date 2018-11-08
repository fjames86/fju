
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

static void messagebox_response( GtkDialog *wnd, gint response_id, gpointer data ) {
  int *dialogresult = (int *)data;
  *dialogresult = response_id;
}

int fju_messagebox_error( GtkWindow *parent, char *fmt, ... ) {
  GtkWidget *wnd;
  int dialogresult = -1;
  va_list args;
  gchar *gstr;
  
  va_start( args, fmt );
  gstr = g_markup_vprintf_escaped( fmt, args );
  va_end( args );
  
  wnd = gtk_message_dialog_new( GTK_WINDOW(parent), GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR, GTK_BUTTONS_OK,
				"%s", gstr );
  gtk_window_set_title( GTK_WINDOW(wnd), "Error" );
  g_signal_connect( wnd, "response", (GCallback)messagebox_response, &dialogresult );
  gtk_dialog_run( GTK_DIALOG(wnd) );
  gtk_widget_destroy( wnd );

  g_free( gstr );

  return dialogresult;
}


int fju_messagebox_okcancel( GtkWindow *parent, char *fmt, ... ) {
  GtkWidget *wnd;
  int dialogresult = 0;
  va_list args;
  gchar *gstr;
  
  va_start( args, fmt );
  gstr = g_markup_vprintf_escaped( fmt, args );
  va_end( args );
  
  wnd = gtk_message_dialog_new( GTK_WINDOW(parent), GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
				"%s", gstr );
  gtk_dialog_add_button( GTK_DIALOG(wnd), "OK", GTK_RESPONSE_OK );
  gtk_dialog_add_button( GTK_DIALOG(wnd), "Cancel", GTK_RESPONSE_CANCEL );
  g_signal_connect( wnd, "response", (GCallback)messagebox_response, &dialogresult );
  gtk_dialog_run( GTK_DIALOG(wnd) );
  gtk_widget_destroy( wnd );
  
  g_free( gstr );

  return dialogresult;
}

int fju_messagebox_yesno( GtkWindow *parent, char *fmt, ... ) {
  GtkWidget *wnd;
  int dialogresult = 0;
  va_list args;
  gchar *gstr;
  
  va_start( args, fmt );
  gstr = g_markup_vprintf_escaped( fmt, args );
  va_end( args );
  
  wnd = gtk_message_dialog_new( GTK_WINDOW(parent), GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
				"%s", gstr );
  gtk_dialog_add_button( GTK_DIALOG(wnd), "Yes", GTK_RESPONSE_YES );
  gtk_dialog_add_button( GTK_DIALOG(wnd), "No", GTK_RESPONSE_NO );
  g_signal_connect( wnd, "response", (GCallback)messagebox_response, &dialogresult );
  gtk_dialog_run( GTK_DIALOG(wnd) );
  gtk_widget_destroy( wnd );

  g_free( gstr );

  return dialogresult;
}
