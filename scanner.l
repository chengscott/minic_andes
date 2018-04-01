%{
#include <stdio.h>
#include <string.h>

int line_number = 0,
  is_newline = 1,
  source_on = 1,
  token_on = 1;
char line[1024];

void print_token(const char *type) {
  if (source_on) printf("#%s:%s\n", type, yytext);
}

void error_handler(const char *error_message) {
  fprintf(stderr, "Error at line %d: %s\n", line_number, error_message);
  exit(1);
}
%}

S [ \t]+
Stdio "fclose"|"clearerr"|"feof"|"ferror"|"fflush"|"fgetpos"|"fopen"|"fread"|"freopen"|"fseek"|"fsetpos"|"ftell"|"fwrite"|"remove"|"rename"|"rewind"|"setbuf"|"setvbuf"|"tmpfile"|"tmpnam"|"fprintf"|"printf"|"sprintf"|"vfprintf"|"vprintf"|"vsprintf"|"fscanf"|"scanf"|"sscanf"|"fgetc"|"fgets"|"fputc"|"fputs"|"getc"|"getchar"|"gets"|"putc"|"putchar"|"puts"|"ungetc"|"perror"
Keyword "void"|"int"|"double"|"bool"|"char"|"null"|"for"|"while"|"do"|"if"|"else"|"switch"|"return"|"break"|"continue"|"const"|"true"|"false"|"struct"|"case"|"default"|{Stdio}
Identifier [a-zA-Z_][a-zA-Z_0-9]*
Operator "+"|"-"|"*"|"/"|"%"|"++"|"--"|"<"|"<="|">"|">="|"=="|"!="|"="|"&&"|"||"|"!"|"&"
Punctuation ":"|";"|","|"."|"\["|"\]"|"\("|"\)"|"\{"|"\}"
Integer [1-9][0-9]*|0
Double (([0-9]*"."[0-9]+)|([0-9]+"."[0-9]*))
Char '(\\.|[^\n])'
Scientific ({Integer}|{Double})[Ee][+-]?{Integer}
String \"[^\n]*\"
PragmaSourceOn "#pragma"{S}"source"{S}"on"
PragmaSourceOff "#pragma"{S}"source"{S}"off"
PragmaTokenOn "#pragma"{S}"token"{S}"on"
PragmaTokenOff "#pragma"{S}"token"{S}"off"

%x Comment
%x SingleComment
%%

{PragmaSourceOn} { source_on = 1; }
{PragmaSourceOff} { source_on = 0; }
{PragmaTokenOn} { token_on = 1; }
{PragmaTokenOff} { token_on = 0; }

"//".* { BEGIN(SingleComment); }
"/*"[^"*/"\n]* { BEGIN(Comment); }
<Comment>. {}
<Comment>"*/" { BEGIN(INITIAL); }

{Keyword} { print_token("key"); }
{Identifier} { print_token("id"); }
{Operator} { print_token("op"); }
{Punctuation} { print_token("punc"); }
{Integer} { print_token("integer"); }
{Double} { print_token("double"); }
{Char} { print_token("char"); }
{Scientific} { print_token("sci"); }

{String} {
  if (token_on) {
    char res[1024];
    strncpy(res, yytext + 1, yyleng - 2);
    res[yyleng - 2] = '\0';
    printf("#string:%s\n", res);
  }
}

[ \t]+ {}

<SingleComment>\n {
  ++line_number;
  is_newline = 1;
  if (source_on) printf("%d:%s\n", line_number, line);
  line[0] = '\0';
  BEGIN(INITIAL);
}

<*>\n {
  ++line_number;
  is_newline = 1;
  if (source_on) printf("%d:%s\n", line_number, line);
  line[0] = '\0';
}

<*>.*/\n {
  if (is_newline) {
    strcpy(line, yytext);
    is_newline = 0;
  }
  REJECT;
}

. { error_handler(yytext); }
%%

int main(void) {
  yylex();
  return 0;
}
