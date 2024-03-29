%option noinput nounput
%{
#include "symbol_table.h"
#include "y.tab.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int line_number = 0,
  pos = 0,
  is_newline = 1,
  source_on = 1,
  token_on = 1;
char line[1024];

int print_token(const char *type, int term) {
  pos += yyleng;
  if (token_on) printf("#%s:%s\n", type, yytext);
  return term;
}
%}

S [ \t]+
Integer [1-9][0-9]*|0
Double (([0-9]*"."[0-9]+)|([0-9]+"."[0-9]*))
Sci ({Integer}|{Double})[Ee][+-]?{Integer}
Char '(\\.|[^\n\'])?'
String \"[^\n\"]*\"
Identifier [a-zA-Z_][a-zA-Z_0-9]*
Punctuation ":"|";"|","|"."|"\["|"\]"|"\("|"\)"|"\{"|"\}"
Operator "+"|"-"|"*"|"/"|"%"|"="|"!"|"<"|">"
PragmaSourceOn "#pragma"{S}"source"{S}"on"
PragmaSourceOff "#pragma"{S}"source"{S}"off"
PragmaTokenOn "#pragma"{S}"token"{S}"on"
PragmaTokenOff "#pragma"{S}"token"{S}"off"
ArduinoHeader "#include"{S}("<arduino.h>"|"<Arduino.h>")

%x Comment
%x SingleComment
%%
{PragmaSourceOn} { pos += yyleng, source_on = 1; }
{PragmaSourceOff} { pos += yyleng, source_on = 0; }
{PragmaTokenOn} { pos += yyleng, token_on = 1; }
{PragmaTokenOff} { pos += yyleng, token_on = 0; }

{ArduinoHeader} { pos += yyleng; }
"OUTPUT" { pos += yyleng, yylval.ival = 1; return INTEGER; }
"HIGH" { pos += yyleng, yylval.ival = 1; return INTEGER; }
"LOW" { pos += yyleng, yylval.ival = 0; return INTEGER; }

"//".* { pos += yyleng; BEGIN(SingleComment); }
"/*"[^"*/"\n]* { pos += yyleng; BEGIN(Comment); }
<Comment>. { pos += yyleng; }
<Comment>"*/" { pos += yyleng; BEGIN(INITIAL); }

"int" {
  yylval.ival = 1;
  return print_token("key", TYPESPEC);
}
"double" {
  yylval.ival = 2;
  return print_token("key", TYPESPEC);
}
"bool" {
  yylval.ival = 3;
  return print_token("key", TYPESPEC);
}
"char" {
  yylval.ival = 4;
  return print_token("key", TYPESPEC);
}
{Identifier} {
  int ret = id_or_keyword(yytext);
  if (ret == IDENTIFIER) {
    yylval.id = (char *) malloc(yyleng);
    strcpy(yylval.id, yytext);
    return print_token("id", IDENTIFIER);
  }
  return print_token("key", ret);
}

{Punctuation} { return (char) print_token("punc", *yytext); }
{Operator} { return (char) print_token("op", *yytext); }
"++" { return print_token("op", PLUSPLUS); }
"--" { return print_token("op", MINUSMINUS); }
"==" { return print_token("op", EQEQ); }
"!=" { return print_token("op", NEQ); }
">=" { return print_token("op", GEQ); }
"<=" { return print_token("op", LEQ); }
"&&" { return print_token("op", OROR); }
"||" { return print_token("op", ANDAND); }

{Integer} {
  yylval.ival = atoi(yytext);
  return print_token("integer", INTEGER);
}
{Double}|{Sci} {
  yylval.dval = atof(yytext);
  return print_token("double", DOUBLE);
}
{Char} {
  yylval.cval = yytext[1];
  return print_token("char", CHAR);
}
{String} {
  yylval.id = (char *) malloc(yyleng);
  strcpy(yylval.id, yytext);
  return print_token("string", STRING);
}
"true"|"false" {
  yylval.ival= yytext[0] == 't' ? 1 : 0;
  return print_token("bool", BOOL);
}

{S} { pos += yyleng; }

<SingleComment>\n {
  ++line_number;
  is_newline = 1;
  if (source_on) printf("%d:%s\n", line_number, line);
  line[0] = '\0';
  BEGIN(INITIAL);
}

<INITIAL,Comment>\n {
  ++line_number;
  is_newline = 1;
  if (source_on) printf("%d:%s\n", line_number, line);
  line[0] = '\0';
}

<*>.*/\n {
  if (is_newline) {
    strcpy(line, yytext);
    pos = 0;
    is_newline = 0;
  }
  REJECT;
}

. { yyerror(""); }

%%

int id_or_keyword(char *rhs) {
  typedef struct {
     char *name;
     unsigned int value;
  } KWORD;

  KWORD keyword_table[] = {
    { "const", TYPEMOD },
    { "void", VOID },
    { "for", FOR },
    { "while", WHILE },
    { "do", DO },
    { "if", IF },
    { "else", ELSE },
    { "switch", SWITCH },
    { "return", RETURN },
    { "break", BREAK },
    { "continue", CONTINUE },
    { "const", TYPEMOD },
    { "case", CASE },
    { "default", DEFAULT_TOKEN },
  };
  size_t i = 0, len = sizeof(keyword_table) / sizeof(KWORD);
  for (i = 0; i < len; ++i) {
    if (!strcmp(rhs, keyword_table[i].name))
      return keyword_table[i].value;
  }
  return IDENTIFIER;
}

int yywrap() { return 1; }
