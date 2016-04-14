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
size_t string_buf_index = 0;

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

DIGIT		[0-9]
LETTER		[a-zA-Z]
WHITESPACE 	" "|"\n"|"\f"|"\r"|"\t"|"\v"
ONE_LN_COMMENT 	"--".*\n
OPEN_COMMENT  	"(*"
CLOSE_COMMENT 	"*)"
CLASS    	(C|c)(L|l)(A|a)(S|s)(S|s)
ELSE   		(E|e)(L|l)(S|s)(E|e)
FI   		(F|f)(I|i)
IF   		(I|i)(F|f)
IN   		(I|i)(N|n)
INHERITS 	(I|i)(N|n)(H|h)(E|e)(R|r)(I|i)(T|t)(S|s)
LET    		(L|l)(E|e)(T|t)
LOOP   		(L|l)(O|o)(O|o)(P|p)
POOL   		(P|p)(O|o)(O|o)(L|l)
THEN   		(T|t)(H|h)(E|e)(N|n)
WHILE    	(W|w)(H|h)(I|i)(L|l)(E|e)
CASE   		(C|c)(A|a)(S|s)(E|e)
ESAC   		(E|e)(S|s)(A|a)(C|c)
OF     		(O|o)(F|f)
NEW		(N|n)(E|e)(W|w)
ISVOID   	(I|i)(S|s)(V|v)(O|o)(I|i)(D|d)
STR_CONST  	\"(.*)\"
INT_CONST  	{DIGIT}+
TRUE   		t(R|r)(U|u)(E|e)
FALSE    	f(A|a)(L|l)(S|s)(E|e)
TYPEID   	[A-Z]({LETTER}|{DIGIT}|_)*
OBJECTID 	[a-z]({LETTER}|{DIGIT}|_)*
ASSIGN   	<-
NOT    		(N|n)(O|o)(T|t)
LE   		<=
DARROW 		=>
NEWLINE  	\n
ERROR    	.

 /* Start conditions used for lexing.
  * normal - inclusive and indicates no special condition
  * comment - exclusive and indicates that we are currently in a comment.
  * string - exclusive and indicates that we are currently in a string.
  * invalid_string - exclusive and indicates that we are in a string that
  *                  has already been invalidated due to a bad character
  *                  or length.
  */
%s normal
%x comment string invalid_string

%%

 /*
  *  RULES SECTION
  */

 /*
  * One line comments should be skipped and the line number updated.
  */

{ONE_LN_COMMENT} { curr_lineno++; } 

 /*
  *  The multiple-character operators.
  */

{DARROW}  { return (DARROW); }

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

 /* Enter string mode when first " is encountered */
\" {
   BEGIN(string);
   string_buf_index = 0;
}

 /* Enter normal mode when second " is encountered. */
<string>\" {
   BEGIN(normal);
   string_buf[string_buf_index] = 0;
   cool_yylval.symbol = stringtable.add_string(string_buf);
   return STR_CONST;
}

 /* Handle special characters within strings. */ 
<string>\\. {
  char ch = yytext[1];
  if (ch == 'n')
    string_buf[string_buf_index++] = '\n';
  else if (ch == 't')
    string_buf[string_buf_index++] = '\t';    
  else if (ch == 'b')
    string_buf[string_buf_index++] = '\b';    
  else if (ch == 'f')
    string_buf[string_buf_index++] = '\f';    
  else
    string_buf[string_buf_index++] = ch;
  if (string_buf_index == MAX_STR_CONST) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN(invalid_string);
    return ERROR;
  }
}

 /* Catch null character within string error. */
<string>'\0' {
  cool_yylval.error_msg = "String contains null character";
  BEGIN(invalid_string);
  return ERROR;
}

 /* Ensure newlines are escaped within strings. */
<string>{NEWLINE} {
  /* Check for escaped newlines. */
  if (string_buf[string_buf_index - 1] == '\\') {
    string_buf_index--;
    string_buf[string_buf_index++] = '\n';
    curr_lineno++;
    /* Check to ensure string is not too long. */
    if (string_buf_index == MAX_STR_CONST) {
       cool_yylval.error_msg = "String constant too long";
       BEGIN(invalid_string);
       return ERROR;
    }     
  } else {
    cool_yylval.error_msg = "Unterminated string constant";
    curr_lineno++;
    BEGIN(normal);
    return ERROR;
  }
}

 /* Catch EOF within string error. */
<string><<EOF>> {
  cool_yylval.error_msg = "Unterminated string constant";
  BEGIN(normal);  
  return ERROR;
}

 /* All characters not accounted for above are just added to the string. */
<string>. {
  string_buf[string_buf_index++] = yytext[0];
  if (string_buf_index == MAX_STR_CONST) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN(invalid_string);
    return ERROR;
  }
}

 /* Updated line number when escaped newline is encountered within
  * an invalid string.
  */
<invalid_string>\\{NEWLINE} {
  curr_lineno++;
}

 /* Leave invalid_string start condition once first unescaped newline
  * or end quotes are found. 
  */
<invalid_string>{NEWLINE} {
  curr_lineno++;
  BEGIN(normal);
}

<invalid_string>\" {
  BEGIN(normal);
}

 /* Ignore charaters of invalid strings until we reach the end. */
<invalid_string>. { }

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
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST; 
}

 /*
  * Identifiers
  */

{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID; 
}

{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

 /* Comment Handling Section. */

 /* Track comment depth to handle nested comments. */
<comment>{OPEN_COMMENT} {  comment_depth++;  }

 /* Enter comment start condition when first (* encountered. */
{OPEN_COMMENT} {  
  BEGIN(comment);
  comment_depth++;
}

 /* Account for newlines in comments. */
<comment>{NEWLINE} {
  curr_lineno++;
}

 /* Catch invalid EOFs within comments. */
<comment><<EOF>> {
  cool_yylval.error_msg = "EOF in comment";
  BEGIN(normal);
  return ERROR;
}

 /* Decrement comment depth when *) encountered and leave
  * comment mode if comment_depth reaches 0.
  */
<comment>{CLOSE_COMMENT} {
  comment_depth--;
  if (comment_depth == 0) BEGIN(normal);
}

 /*
  * If *) encountered when not in a comment, return
  * error.
  */
{CLOSE_COMMENT} {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}

 /* Ignore actual characters within comments. */
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
"=" { return '='; }
"/" { return '/'; }
"~" { return '~'; }
"<" { return '<'; }
"(" { return '('; }
")" { return ')'; }
":" { return ':'; }
";" { return ';'; }
"," { return ','; }
"{" { return '{'; }
"}" { return '}'; }

 /* Ignore whitespace. */
{WHITESPACE} {}

 /*
  * Error-catching rule. Will match any character that
  * does not match any other rule while in the normal
  * start condition.
  */

{ERROR} {
  cool_yylval.error_msg = yytext;
  return ERROR;
}

%%
