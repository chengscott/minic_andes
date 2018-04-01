%{
#include <stdio.h>
#include <string.h>

int line_number = 0;
int is_newline = 1;
char line[1024];
%}

Stdio "fclose"|"clearerr"|"feof"|"ferror"|"fflush"|"fgetpos"|"fopen"|"fread"|"freopen"|"fseek"|"fsetpos"|"ftell"|"fwrite"|"remove"|"rename"|"rewind"|"setbuf"|"setvbuf"|"tmpfile"|"tmpnam"|"fprintf"|"printf"|"sprintf"|"vfprintf"|"vprintf"|"vsprintf"|"fscanf"|"scanf"|"sscanf"|"fgetc"|"fgets"|"fputc"|"fputs"|"getc"|"getchar"|"gets"|"putc"|"putchar"|"puts"|"ungetc"|"perror"
Keyword "void"|"int"|"double"|"bool"|"char"|"null"|"for"|"while"|"do"|"if"|"else"|"switch"|"return"|"break"|"continue"|"const"|"true"|"false"|"struct"|"case"|"default"|{Stdio}
Identifier [a-zA-Z_][a-zA-Z_0-9]*
Operator "+"|"-"|"*"|"/"|"%"|"++"|"--"|"<"|"<="|">"|">="|"=="|"!="|"="|"&&"|"||"|"!"|"&"
Punctuation ":"|";"|","|"."|"\["|"\]"|"\("|"\)"|"\{"|"\}"
Integer [1-9][0-9]*|0
Double (([0-9]*"."[0-9]+)|([0-9]+"."[0-9]*))
Char \'[^\n]\'
Scientific ({Integer}|{Double})[Ee][+-]?{Integer}
String \"[^\n]*\"

%x Comment
%x SingleComment
%%

"//".* { BEGIN(SingleComment); }
"/*".* { BEGIN(Comment); }
<Comment>. {}
<Comment>"*/" { BEGIN(INITIAL); }

{Keyword} {
  printf("#key:%s\n", yytext);
}

{Identifier} {
  printf("#id:%s\n", yytext);
}

{Operator} {
  printf("#op:%s\n", yytext);
}

{Punctuation} {
  printf("#punc:%s\n", yytext);
}

{Integer} {
  printf("#integer:%s\n", yytext);
}

{Double} {
  printf("#double:%s\n", yytext);
}

{Char} {
  printf("#char:%s\n", yytext);
}

{Scientific} {
  printf("#sci:%s\n", yytext);
}

{String} {
  char res[1024];
  strncpy(res, yytext + 1, yyleng - 2);
  res[yyleng - 2] = '\0';
  printf("#string:%s\n", res);

}

[ \t]+ {}

<SingleComment>\n {
  ++line_number;
  is_newline = 1;
  printf("%d:%s\n", line_number, line);
  line[0] = '\0';
  BEGIN(INITIAL);
}

<*>\n {
  ++line_number;
  is_newline = 1;
  printf("%d:%s\n", line_number, line);
  line[0] = '\0';
}

<*>.*/\n {
  if (is_newline) {
    strcpy(line, yytext);
    is_newline = 0;
  }
  REJECT;
}
%%

int main(void) {
  yylex();
  return 0;
}

void error_handler(const char *error_message) {
  fprintf(stderr, "Error at line %d: %s\n", line_number, error_message);
  exit(1);
}
