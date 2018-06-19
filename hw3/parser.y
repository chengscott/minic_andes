%{
#include "symbol_table.h"
#include <stdio.h>
#include <stdlib.h>
extern int line_number, pos;
extern char *yytext;
extern char line[1024];
FILE *f_asm;
int cur_scope = 0,
    cur_offset = 0;
int stack_offset = 0;
int cur_label = 0;
int expr_cnt = 0; // count function parameters (expr)
#define DEBUG
#ifdef DEBUG
#define DBG(fmt) fprintf(f_asm, fmt);
#else
#define DBG(fmt)
#endif
int has_entry_function = 0, nofunc = 0, is_loop = 0, is_switch = 0;
void print_asm(char *instr, char *content) {
  fprintf(f_asm, "\t%-8s%s\n", instr, content);
}
void printd_asm(char *instr, char *front, int d, char *end) {
  fprintf(f_asm, "\t%-8s%s%d%s\n", instr, front, d, end);
}
void prints_asm(char *instr, char *front, char *s, char *end) {
  fprintf(f_asm, "\t%-8s%s%s%s\n", instr, front, s, end);
}
void push(int reg) {
  --stack_offset;
  fprintf(f_asm, "\t%-8s$r%d, [$sp + %d]\n", "swi", reg, stack_offset * 4);
}
void push_offset(int reg, int offset) {
  fprintf(f_asm, "\t%-8s$r%d, [$sp + %d]\n", "swi", reg, offset * 4);
}
void pop(int reg) {
  fprintf(f_asm, "\t%-8s$r%d, [$sp + %d]\n", "lwi", reg, stack_offset * 4);
  ++stack_offset;
}
void enter_scope() {
  ++cur_scope;
}
void exit_scope() {
  --cur_scope;
}
%}

%start program
%union {
  int ival;
  double dval;
  char cval;
  char *id;
}

/* all identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token <id> IDENTIFIER STRING
%token <ival> INTEGER BOOL
%token <dval> DOUBLE
%token <cval> CHAR

/* reserved words that specify storage class.
%token SCSPEC*/

/* reserved words that specify type. */
%token <ival> TYPESPEC

/* reserved words that modify type: "const" or "volatile". */
%token TYPEMOD

/* the reserved words */
%token FOR WHILE DO IF ELSE SWITCH RETURN BREAK CONTINUE CASE DEFAULT_TOKEN VOID

%type <id> IDENTIFIER fdec
%type <ival> primary expr_no_commas expr

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator. */
%left ';'
%left IDENTIFIER TYPESPEC TYPEMOD
%left ','
%right '='
%left OROR
%left ANDAND
%right '!'
%left '<' '>' LEQ GEQ EQEQ NEQ
%left '+' '-'
%left '*' '/' '%'
%right UNARY
%left PLUSPLUS MINUSMINUS
%left '.' '(' '[' '{'

%%
program: extdefs {
           // must have at least one global function definition
           if (!has_entry_function) yyerror("");
           if (!has_function_def()) yyerror("");
           printf("No syntax error!\n");
         }
       ;

extdefs: extdef
       | extdefs extdef
       ;

extdef: decl
      | fdecl
      /* function definition */
      | TYPESPEC fdec {
          has_entry_function = 1;
          int idx = find_func($2);
          ftable[idx].has_def = 1;
        } stmtblk
      | VOID fdec {
          has_entry_function = 1;
          int idx = find_func($2);
          ftable[idx].has_def = 1;
        } stmtblk
      ;

// expression
xexpr: /*empty */
     | expr
     ;

expr: expr_no_commas { ++expr_cnt; $$ = $1; }
    | expr ',' expr_no_commas { ++expr_cnt; $$ = $1; }
    ;

expr_no_commas: primary
              | IDENTIFIER {
                  if (find_symbol($1) == -1) yyerror("");
                } '=' expr_no_commas {
                  DBG("# ID = expr\n");
                  int idx = find_symbol($1);
                  SYMPTR entry = table + idx;
                  pop(0);
                  printd_asm("swi", "$r0, [$sp + ", entry->offset * 4, "]");
                  push(0);
                  $$ = entry->type;
                }
              | '!' expr_no_commas {
                  $$ = $2;
                  // (!expr) equals (expr == 0)
                  pop(0);
                  print_asm("xori", "$r0, $r0, 0");
                  print_asm("slti", "$r0, $r0, 1");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas EQEQ expr_no_commas {
                  if ($1 != $3) yyerror("");
                  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("xor", "$r0, $r0, $r1");
                  print_asm("slti", "$r0, $r0, 1");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas NEQ expr_no_commas {
                  if ($1 != $3) yyerror("");
                  $$ = $1;
                  // (expr != expr) equals ~(expr == expr)
                  DBG("# expr != expr\n");
                  pop(1);
                  pop(0);
                  print_asm("xor", "$r0, $r0, $r1");
                  print_asm("slti", "$r0, $r0, 1");
                  print_asm("xori", "$r0, $r0, 1");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas '<' expr_no_commas {
                  if ($1 != $3) yyerror("");
                  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("slts", "$r0, $r0, $r1");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas '>' expr_no_commas {
                  if ($1 != $3) yyerror("");
                  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("slts", "$r0, $r1, $r0");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas LEQ expr_no_commas {
                  if ($1 != $3) yyerror("");
                  $$ = $1;
                  // (expr <= expr) equals ~(expr > expr)
                  pop(1);
                  pop(0);
                  print_asm("slts", "$r0, $r1, $r0");
                  print_asm("xori", "$r0, $r0, 1");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas GEQ expr_no_commas {
                  if ($1 != $3) yyerror("");
                  $$ = $1;
                  // (expr >= expr) equals ~(expr < expr)
                  pop(1);
                  pop(0);
                  print_asm("slts", "$r0, $r0, $r1");
                  print_asm("xori", "$r0, $r0, 1");
                  print_asm("zeb", "$r0, $r0");
                  push(0);
                }
              | expr_no_commas '+' expr_no_commas {
                  DBG("# expr + expr\n");
                  if ($1 != $3) yyerror("");
		  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("add", "$r0, $r0, $r1");
                  push(0);
                }
              | expr_no_commas '-' expr_no_commas {
                  if ($1 != $3) yyerror("");
		  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("sub", "$r0, $r0, $r1");
                  push(0);
                }
              | expr_no_commas '*' expr_no_commas {
                  DBG("# expr * expr\n");
                  if ($1 != $3) yyerror("");
		  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("mul", "$r0, $r0, $r1");
                  push(0);
                }
              | expr_no_commas '/' expr_no_commas {
                  if ($1 != $3) yyerror("");
		  $$ = $1;
                  pop(1);
                  pop(0);
                  print_asm("divsr", "$r0, $r3, $r0, $r1");
                  push(0);
                }
              | '-' expr_no_commas %prec UNARY {
		  $$ = $2;
                  pop(0);
                  print_asm("xor", "$r1, $r1, $r1");
                  print_asm("sub", "$r0, $r0, $r1");
                  push(0);
                }
              ;

primary: IDENTIFIER {
           DBG("# ID\n");
           int idx = find_symbol($1);
           if (idx == -1) yyerror("");
           SYMPTR entry = table + idx;
           printd_asm("lwi", "$r0, [$sp + ", entry->offset * 4, "]");
           push(0);
           $$ = entry->type;
         }
       | INTEGER {
           DBG("# INT\n");
           $$ = 1;
           printd_asm("movi", "$r0, ", $1, "");
           push(0);
         }
       | DOUBLE { $$ = 2; }
       | CHAR { $$ = 3; }
       | BOOL { $$ = 4; }
       /* function invocation */
       | IDENTIFIER {
           expr_cnt = 0;
           if (nofunc) yyerror("");
           if (find_func($1) == -1) yyerror("");
         }
         '(' xexpr ')' {
           DBG("# ID(expr)\n");
           $$ = ftable[find_func($1)].type;
           while (expr_cnt--)
             pop(expr_cnt);
           prints_asm("bal", $1, "", "");
           push(0);
         }
       | '(' expr ')' { --expr_cnt; $$ = $2; }
       ;

// statement
stmtblk: '{' { enter_scope(); }
         decls stmts { exit_scope(); } '}'
       ;

stmts: /* empty */
     | stmts stmt
     ;

stmt: xexpr ';'
    | stmtblk
    | SWITCH '(' IDENTIFIER ')' '{'
      { is_switch = 1; } cases_blk default_blk { is_switch = 0; }
      '}'
    | FOR '(' xexpr ';' xexpr ';' xexpr ')'
      { is_loop = 1; } stmtblk { is_loop = 0; }
    | WHILE { fprintf(f_asm, ".L%d:\n", cur_label); }
      '(' expr ')' '{' {
        pop(0);
        printd_asm("beqz", "$r0, .L", cur_label + 1, "");
        enter_scope();
        is_loop = 1;
      }
      decls stmts {
        is_loop = 0;
        exit_scope();
        printd_asm("j", ".L", cur_label, "");
        fprintf(f_asm, ".L%d:\n", cur_label + 1);
        cur_label += 2;
      }
      '}'
    | DO '{' {
        fprintf(f_asm, ".L%d:\n", cur_label);
        enter_scope();
        is_loop = 1;
      }
      decls stmts {
        is_loop = 0;
        exit_scope();
      }
      '}' WHILE '(' expr_no_commas ')' ';' {
        pop(0);
        printd_asm("bnez", "$r0, .L", cur_label, "");
        ++cur_label;
      }
    | IF '(' expr ')' '{' {
        pop(0);
        printd_asm("beqz", "$r0, .L", cur_label, "");
        enter_scope();
      }
      decls stmts { exit_scope(); }
      '}' else_stmtblk
    | BREAK ';' { if (!is_loop && !is_switch) yyerror(""); }
    | CONTINUE ';' { if (!is_loop) yyerror(""); }
    | RETURN expr ';'
    ;

else_stmtblk: /* empty */ {
                fprintf(f_asm, ".L%d:\n", cur_label++);
              }
            | ELSE '{' {
                printd_asm("j", ".L", cur_label + 1, "");
                enter_scope();
                fprintf(f_asm, ".L%d:\n", cur_label);
              }
              decls stmts {
                exit_scope();
                fprintf(f_asm, ".L%d:\n", cur_label + 1);
                cur_label += 2;
              }
              '}'
            ;

cases_blk: case_blk cases_blk
         ;

case_blk: CASE INTEGER ':' stmts
        | CASE CHAR ':' stmts
        ;

default_blk: /* empty */
           | DEFAULT_TOKEN ':' stmts
           ;

// function declaration
fdecl: TYPESPEC fdecs ';' { update_func($1); }
     | VOID fdecs ';' { update_func(0); }
     ;

fdecs: fdecs ',' fdec
     | fdec {
         int idx = find_func($1);
         if (ftable[idx].decs > 1) yyerror("");
       }
     ;

fdec: IDENTIFIER {
        int idx = find_func($1);
        if (idx == -1) add_func(-1, $1, 0);
        else if (ftable[idx].has_def) yyerror("");
        else ++ftable[idx].decs;
      }
      '(' parmlist ')' { $$ = $1; }
    ;

parmlist: /* empty */
        | parms
        ;

parms: parms ',' parm
     | parm
     ;

parm: TYPESPEC IDENTIFIER
    ;

// variable declaration
decls: /* empty */
     | decls decl
     ;

decl: TYPESPEC decs ';' {
        if (update_symbol($1) == -1) yyerror("");
      }
    ;

decs: decs ',' dec
    | dec
    ;

dec: IDENTIFIER {
       if (find_symbol($1) != -1) yyerror("");
       add_symbol($1, cur_scope, cur_offset++);
     }
   | IDENTIFIER {
       DBG("# ID = expr\n");
       if (find_symbol($1) != -1) yyerror("");
       add_symbol($1, cur_scope, cur_offset++);
     }
     '=' { nofunc = 1; }
     expr_no_commas {
       int idx = find_symbol($1);
       SYMPTR entry = table + idx;
       entry->init_type = $5;
       pop(0);
       push_offset(0, entry->offset);
       nofunc = 0;
     }
   ;
%%

void yyerror(char *s) {
  line[pos] = '\0';
  fprintf(stderr, "*** Error at line %d: %s\n", ++line_number, line);
  fprintf(stderr, "\n");
  fprintf(stderr, "Unmatched token: %s\n", yytext);
  fprintf(stderr, "*** syntax error\n");
  exit(-1);
}

int main(void) {
  f_asm = fopen("assembly", "w");
  // PROLOGUE
  add_func(0, "digitalWrite", 1);
  add_func(0, "delay", 1);
  yyparse();
  // EPILOGUE
  return 0;
}
