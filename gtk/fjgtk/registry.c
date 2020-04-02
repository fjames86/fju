
#include <stdlib.h>
#include <string.h>

#include <fju/fjgtk.h>

#define FJGTK_MAX_ENTRY 1024

struct entry {
  struct entry *next;
  GtkWidget *hwnd;
  char *name;
  int id;
};

static struct {
  struct entry *entry;
  int idx;
  struct entry etab[FJGTK_MAX_ENTRY];
} glob;

int fjgtk_register( GtkWidget *hwnd, char *name ) {
  struct entry *e = &glob.etab[glob.idx];
  e->id = glob.idx;
  glob.idx++;
  e->hwnd = hwnd;
  e->name = name;
  e->next = glob.entry;
  glob.entry = e;
  return e->id;
}
  
GtkWidget *fjgtk_get( char *name ) {
  struct entry *e;
  e = glob.entry;
  while( e ) {
    if( strcmp( e->name, name ) == 0 ) return e->hwnd;
    e = e->next;
  }
  return NULL;
}

GtkWidget *fjgtk_get_by_id( int id ) {
  struct entry *e;
  e = glob.entry;
  while( e ) {
    if( e->id == id ) return e->hwnd;
    e = e->next;
  }
  return NULL;
}


