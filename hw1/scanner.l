%option noinput nounput
%{
#include <stdio.h>
#include <string.h>

int line_number = 0,
  is_newline = 1,
  source_on = 1,
  token_on = 1;
char line[1024];

void print_token(const char *type) {
  if (token_on) printf("#%s:%s\n", type, yytext);
}
%}

S [ \t]+
Stdio "fclose"|"clearerr"|"feof"|"ferror"|"fflush"|"fgetpos"|"fopen"|"fread"|"freopen"|"fseek"|"fsetpos"|"ftell"|"fwrite"|"remove"|"rename"|"rewind"|"setbuf"|"setvbuf"|"tmpfile"|"tmpnam"|"fprintf"|"printf"|"sprintf"|"vfprintf"|"vprintf"|"vsprintf"|"fscanf"|"scanf"|"sscanf"|"fgetc"|"fgets"|"fputc"|"fputs"|"getc"|"getchar"|"gets"|"putc"|"putchar"|"puts"|"ungetc"|"perror"
Keyword "void"|"int"|"double"|"bool"|"char"|"null"|"for"|"while"|"do"|"if"|"else"|"switch"|"return"|"break"|"continue"|"const"|"true"|"false"|"struct"|"case"|"default"|{Stdio}
Identifier [a-zA-Z_][a-zA-Z_0-9]*
Operator "+"|"-"|"*"|"/"|"%"|"++"|"--"|"<"|"<="|">"|">="|"=="|"!="|"="|"&&"|"||"|"!"
Punctuation ":"|";"|","|"."|"\["|"\]"|"\("|"\)"|"\{"|"\}"
Integer [1-9][0-9]*|0
Double (([0-9]*"."[0-9]+)|([0-9]+"."[0-9]*))
Scientific ({Integer}|{Double})[Ee][+-]?{Integer}
Char '(\\.|[^\n\'])?'
String \"[^\n\"]*\"
PragmaSourceOn "#pragma"{S}"source"{S}"on"
PragmaSourceOff "#pragma"{S}"source"{S}"off"
PragmaTokenOn "#pragma"{S}"token"{S}"on"
PragmaTokenOff "#pragma"{S}"token"{S}"off"
/* According to TA, {Scientific}{Identifier} is valid */
Err {Integer}{Integer}|{Double}{Double}|{Scientific}{Scientific}|{Scientific}{Double}|{Integer}{Identifier}|{Double}{Identifier}|{Identifier}[^ \t\n"+""-""*""/""%""<""="">""!""&""|"":"";"","".""\[""\]""\(""\)""\{""\}"]

%x Comment
%x SingleComment
%x Error
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

"&"{Identifier} { if (token_on) printf("#op:&\n#id:%s\n", yytext + 1); }

[ \t]+ {}

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

<Error>.*\n {
  fprintf(stderr, "%s", yytext);
  exit(1);
}

<*>.*/\n {
  if (is_newline) {
    strcpy(line, yytext);
    is_newline = 0;
  }
  REJECT;
}

.|{Err} {
  fprintf(stderr, "Error at line %d: %s", ++line_number, yytext);
  BEGIN(Error);
}
%%

int main(void) {
  yylex();
  return 0;
}
