#include "symbol_table.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int table_size = 0;
int add_symbol(const char *name, int scope, int offset) {
  SYMPTR entry = table + table_size++;
  entry->type = -1;
  entry->scope = scope;
  entry->name = (char *) malloc(strlen(name) + 1);
  strcpy(entry->name, name);
  entry->offset = offset;
  entry->init_type = -1;
  return offset;
}

int update_symbol(int type) {
  int i;
  for (i = table_size - 1; i >= 0; --i) {
    if (table[i].type == -1) {
      if (table[i].init_type != type && table[i].init_type != -1) return -1;
      table[i].type = type;
    }
  }
  return 0;
}

int find_symbol(const char *name) {
  int i;
  for (i = table_size - 1; i >= 0; --i)
    if (strcmp(table[i].name, name) == 0)
      return i;
  return -1;
}

void pop_symbol(int scope) {
  while (table_size >= 0 && table[table_size - 1].scope > scope)
    --table_size;
}

static int ftable_size = 0;
void add_func(int type, const char *name, int isdef) {
  FUNCPTR entry = ftable + ftable_size++;
  entry->type = type;
  entry->name = (char *) malloc(strlen(name) + 1);
  strcpy(entry->name, name);
  entry->is_param = 0;
  entry->has_def = isdef;
  entry->decs = 1;
}

int find_func(const char *name) {
  int i;
  for (i = ftable_size - 1; i >= 0; --i)
    if (ftable[i].is_param == 0 && strcmp(ftable[i].name, name) == 0)
      return i;
  return -1;
}

void update_func(int type) {
  int i;
  for (i = ftable_size - 1; i >= 0; --i)
    if (ftable[i].type == -1)
      ftable[i].type = type;
}

int has_function_def(void) {
  int i;
  for (i = 0; i < ftable_size; ++i)
    if (!ftable[i].has_def)
      return 0;
  return 1;
}
