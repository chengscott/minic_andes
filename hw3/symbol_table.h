#ifndef SYMTABLEH
#define SYMTABLE_H

#define MAX_TABLE_SIZE 5000

typedef struct symbol_entry *SYMPTR;
struct symbol_entry {
  int type;
  char *name;
  int scope;
  int offset;
  int init_type;
} table[MAX_TABLE_SIZE];

typedef struct func_entry *FUNCPTR;
struct func_entry {
  int type;
  char *name;
  int is_param;
  int has_def;
  int decs;
} ftable[MAX_TABLE_SIZE];

#endif
