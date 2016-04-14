/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;
extern int comment_depth = 0;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * DEFINITIONS SECTION
 * Define names for regular expressions here.
 */

DIGIT  [0-9]
LETTER   ([a-z]|[A-Z])
WHITESPACE " "|"\n"|"\f"|"\r"|"\t"|"\v"
ONE_LN_COMMENT "--".*\n
OPEN_COMMENT  "(*"
CLOSE_COMMENT "*)"
CLASS    (C|c)(L|l)(A|a)(S|s)(S|s)
ELSE   (E|e)(L|l)(S|s)(E|e)
FI   (F|f)(I|i)
IF   (I|i)(F|f)
IN   (I|i)(N|n)
INHERITS (I|i)(N|n)(H|h)(E|e)(R|r)(I|i)(T|t)(S|s)
LET    (L|l)(E|e)(T|t)
LOOP   (L|l)(O|o)(O|o)(P|p)
POOL   (P|p)(O|o)(O|o)(L|l)
THEN   (T|t)(H|h)(E|e)(N|n)
WHILE    (W|w)(H|h)(I|i)(L|l)(E|e)
CASE   (C|c)(A|a)(S|s)(E|e)
ESAC   (E|e)(S|s)(A|a)(C|c)
OF   (O|o)|(F|f)
NEW    (N|n)(E|e)(W|w)
ISVOID   (I|i)(S|s)(V|v)(O|o)(I|i)(D|d)
STR_CONST  \"(.*)\"
INT_CONST  DIGIT+
TRUE   t(R|r)(U|u)(E|e)
FALSE    f(A|a)(L|l)(S|s)(E|e)
BOOL_CONST TRUE | FALSE
TYPEID   [A-Z](LETTER|DIGIT|_)*
OBJECTID [a-z](LETTER|DIGIT|_)*
ASSIGN   <-
NOT    (N|n)(O|o)(T|t)
LE   <=
NEWLINE  \n
ERROR    . 
darrow  =>

%s normal
%x comment string
%%

 /*
  *  RULES SECTION
  */

 /*
  * One line comments should be skipped.
  */

"--".*\n { curr_lineno++; } 

 /*
  *  The multiple-character operators.
  */

{darrow}  { return (DARROW); }

{ASSIGN}  { return (ASSIGN); }

{LE}  { return (LE); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS} { return CLASS; }

{ELSE} { return ELSE; }

{FI} { return FI; }

{IF} { return IF; }

{IN} { return IN; }

{INHERITS} { return INHERITS; }

{LET} { return LET; }

{LOOP} { return LOOP; }

{POOL} { return POOL; }

{THEN} { return THEN; }

{WHILE} { return WHILE; }

{CASE} { return CASE; }

{ESAC} { return ESAC; }

{OF} { return OF; }

{NEW} { return NEW; }

{ISVOID} { return ISVOID; }

{NOT} { return NOT; }

 /*
  * Handle incrementing curr_lineno on every newline.
  */
{NEWLINE} { curr_lineno++; }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
{STR_CONST} {
  printf("\nIN A STR CONST\n");
  cool_yylval.symbol = stringtable.add_string(yytext);
  return STR_CONST; 
}

 /*
  * Boolean and integer constants
  */
{TRUE} {
  cool_yylval.boolean = true;
  return BOOL_CONST;
}

{FALSE} {
  cool_yylval.boolean = false;
  return BOOL_CONST;
}

{INT_CONST} { 
  cool_yylval.symbol = inttable.add_int(atoi(yytext));
  return INT_CONST; 
}

 /*
  * Identifiers
  */

{TYPEID} {  
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID; 
}

{OBJECTID} {  cool_yylval.symbol = idtable.add_string(yytext); }

  /* Comment Handling Section. */

<comment>{OPEN_COMMENT} {  comment_depth++;  }

{OPEN_COMMENT} {  
  BEGIN(comment);
  comment_depth++;
}

<comment>{NEWLINE} {
  printf("Incrementing the line number\n");
  curr_lineno++;
}

<comment><<EOF>> {
  cool_yylval.error_msg = "EOF in comment";
  BEGIN(normal);
  return ERROR;
}

<comment>{CLOSE_COMMENT} {
  comment_depth--;
  if (comment_depth == 0) BEGIN(normal);
}

{CLOSE_COMMENT} {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}

<comment>. {
  /* Do Nothing. */
}

 /*
  * Single character tokens used by COOL.
  */

"." { return '.'; }
"@" { return '@'; }
"+" { return '+'; }
"-" { return '-'; }
"*" { return '*'; }
"/" { return '/'; }
"~" { return '~'; }
"<" { return '<'; }
"(" { return '('; }
")" { return ')'; }
":" { return ':'; }
";" { return ';'; }
"," { return ','; }

{WHITESPACE} {}

 /*
  * Error-catching rule
  */
{ERROR} {
  cool_yylval.error_msg = yytext;
  return ERROR;
}

%%
