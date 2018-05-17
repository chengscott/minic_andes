%option noinput nounput
%{
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
Identifier [a-zA-Z_][a-zA-Z_0-9]*
Punctuation ":"|";"|","|"."|"\["|"\]"|"\("|"\)"|"\{"|"\}"
Operator "+"|"-"|"*"|"/"|"%"|"="|"!"
PragmaSourceOn "#pragma"{S}"source"{S}"on"
PragmaSourceOff "#pragma"{S}"source"{S}"off"
PragmaTokenOn "#pragma"{S}"token"{S}"on"
PragmaTokenOff "#pragma"{S}"token"{S}"off"

%x Comment
%x SingleComment
%%
{PragmaSourceOn} { pos += yyleng, source_on = 1; }
{PragmaSourceOff} { pos += yyleng, source_on = 0; }
{PragmaTokenOn} { pos += yyleng, token_on = 1; }
{PragmaTokenOff} { pos += yyleng, token_on = 0; }

"//".* { pos += yyleng; BEGIN(SingleComment); }
"/*"[^"*/"\n]* { pos += yyleng; BEGIN(Comment); }
<Comment>. { pos += yyleng; }
<Comment>"*/" { pos += yyleng; BEGIN(INITIAL); }

{Identifier} {
  int ret = id_or_keyword(yytext);
  if (ret == IDENTIFIER)
    return print_token("id", IDENTIFIER);
  return print_token("key", ret);
}

{Punctuation} { return (char) print_token("punc", *yytext); }
{Operator} { return (char) print_token("op", *yytext); }
"++" { return print_token("op", PLUSPLUS); }
"--" { return print_token("op", MINUSMINUS); }
"==" { return print_token("op", EQCOMPARE); }
"!=" { return print_token("op", NEQCOMPARE); }
"&&" { return print_token("op", OROR); }
"||" { return print_token("op", ANDAND); }
[<|>]=? { return print_token("op", ARITHCOMPARE); }

{Integer} { return print_token("integer", INTEGER); }
{Double}|{Sci} { return print_token("double", DOUBLE); }
{Char} { return print_token("char", CHAR); }

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
    { "int", TYPESPEC },
    { "double", TYPESPEC },
    { "bool", TYPESPEC },
    { "char", TYPESPEC },
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
    { "true", BOOL },
    { "false", BOOL },
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
