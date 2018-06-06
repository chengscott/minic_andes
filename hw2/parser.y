%{
#include <stdio.h>
#include <stdlib.h>
extern int line_number, pos;
extern char *yytext;
extern char line[1024];
#define DEBUG
#ifdef DEBUG
#define DBG(fmt) printf(fmt);
#else
#define DBG(fmt)
#endif
int has_entry_function = 0, nofunc = 0;
%}

%start program
%union {
  double dval;
  char cval;
  char *id;
}

/* all identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token <id> IDENTIFIER STRING
%token <dval> INTEGER BOOL DOUBLE
%token <cval> CHAR

/* reserved words that specify storage class.
%token SCSPEC*/

/* reserved words that specify type. */
%token TYPESPEC

/* reserved words that modify type: "const" or "volatile". */
%token TYPEMOD

/* the reserved words */
%token FOR WHILE DO IF ELSE SWITCH RETURN BREAK CONTINUE CASE DEFAULT_TOKEN VOID

%type <id> notype_declarator notype_array IDENTIFIER primary expr_no_commas

%type <dval> CONSTANT

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator. */
%left ';'
%left IDENTIFIER SCSPEC TYPESPEC TYPEMOD
%left ','
%right '='
%left <dval> OROR
%left <dval> ANDAND
%right <dval> '!'
%left <dval> ARITHCOMPARE EQCOMPARE NEQCOMPARE
%left <dval> '+' '-'
%left <dval> '*' '/' '%'
%right <dval> UNARY
%left <dval> PLUSPLUS MINUSMINUS
%left '.' '(' '['

%%
CONSTANT: INTEGER
        | DOUBLE
        | BOOL
        | CHAR  { $$ = 0; }
        ;

program: extdefs {
           // must have at least one global function definition
           if (!has_entry_function) yyerror("");
           printf("No syntax error!\n");
         }
       ;

extdefs: extdef
       | extdefs extdef
       ;

extdef: decl
      /* functin declaration */
      | TYPESPEC fdecs ';'
      | VOID fdecs ';'
      /* function definition */
      | TYPESPEC notype_declarator stmtblk { has_entry_function = 1; }
      | VOID notype_declarator stmtblk { has_entry_function = 1; }
      ;

xexpr: /*empty */
     | expr
     ;

expr: expr_no_commas
    | expr ',' expr_no_commas
    ;

expr_no_commas: primary
              | IDENTIFIER '=' expr_no_commas
              | array '=' expr_no_commas { $$ = NULL; }
              | expr_no_commas OROR expr_no_commas
              | expr_no_commas ANDAND expr_no_commas
              | '!' expr_no_commas { $$ = $2; }
              | expr_no_commas EQCOMPARE expr_no_commas
              | expr_no_commas NEQCOMPARE expr_no_commas
              | expr_no_commas ARITHCOMPARE expr_no_commas
              | expr_no_commas '+' expr_no_commas
              | expr_no_commas '-' expr_no_commas
              | expr_no_commas '*' expr_no_commas
              | expr_no_commas '/' expr_no_commas
              | expr_no_commas '%' expr_no_commas
              | '-' expr_no_commas %prec UNARY { $$ = $2; }
              ;

primary: IDENTIFIER
       | CONSTANT { $$ = NULL; }
       /* function invocation */
       | IDENTIFIER '(' { if (nofunc) yyerror(""); } xexpr ')'
       /* array invocation */
       | array { $$ = NULL; }
       | primary PLUSPLUS
       | primary MINUSMINUS
       | '(' expr ')' { $$ = NULL; }
       ;

// notype function declarator
notype_declarator: IDENTIFIER '(' parmlist ')' %prec '.'
                 ;

fdecs: notype_declarator ',' fdecs
     | notype_declarator
     ;

// notype array declarator
notype_array: IDENTIFIER '[' INTEGER ']'
            | notype_array '[' INTEGER ']'
            ;

// array invocation
array: IDENTIFIER '[' expr ']'
     | array '[' expr ']'
     ;

// function parameter list declaration
parmlist: /* empty */
        | parms
        ;

parms: parm
     | parms ',' parm
     ;

parm: TYPESPEC IDENTIFIER
    | TYPESPEC notype_array
    | TYPEMOD TYPESPEC IDENTIFIER
    ;

// statements
stmts: /* empty */
     | stmt stmts
     ;

stmt: xexpr ';'
    | stmtblk
    | IF '(' expr ')' stmtblk else_stmtblk
    | SWITCH '(' IDENTIFIER ')' '{' cases_blk default_blk '}'
    | FOR '(' xexpr ';' xexpr ';' xexpr ')' stmtblk
    | WHILE '(' expr ')' stmtblk
    | DO stmtblk WHILE '(' expr_no_commas ')' ';'
    | BREAK ';'
    | CONTINUE ';'
    | RETURN expr ';'
    ;

stmtblk: '{' decls stmts '}'
       ;

else_stmtblk: ELSE stmtblk
         |
         ;

cases_blk: case_blk cases_blk
         ;

case_blk: CASE INTEGER ':' stmts
        | CASE CHAR ':' stmts
        ;

default_blk: /*empty*/
           | DEFAULT_TOKEN ':' stmts
           ;

// variable declaration
decls: /* empty */
     | decl decls
     ;

decl: TYPESPEC decs ';'
    | TYPEMOD TYPESPEC cdecs ';'
    ;

decs: dec ',' decs
    | dec
    ;

dec: IDENTIFIER
   | IDENTIFIER '=' { nofunc = 1; } expr_no_commas { nofunc = 0; }
   | notype_array
   | notype_array '=' '{' { nofunc = 1; } xexpr { nofunc = 0; } '}'
   ;

cdecs: cdec ',' cdecs
     | cdec
     ;

cdec: IDENTIFIER '=' CONSTANT
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
  yyparse();
  return 0;
}
