%%
%{
// $Id$

// ----------------------------------------------------------------------------
//  ISCO is Copyright (C) 1998-2001 Salvador Abreu
//  
//     This program is free software; you can redistribute it and/or
//     modify it under the terms of the GNU General Public License as
//     published by the Free Software Foundation; either version 2, or
//     (at your option) any later version.
//  
//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//     General Public License for more details.
//  
//     You should have received a copy of the GNU General Public License
//     along with this program; if not, write to the Free Software
//     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//     02111-1307, USA.
//  
//  On Debian GNU/Linux systems, the complete text of the GNU General
//  Public License can be found in `/usr/share/common-licenses/GPL'.
// ----------------------------------------------------------------------------

  private java_cup.runtime.Symbol tok(int k, Object o){
    return new java_cup.runtime.Symbol(k,yychar,yychar+yylength(),o);
  }
%} 

%class Scanner
%function nextToken
%type java_cup.runtime.Symbol

%char
%full
%ignorecase

%eofval{
  return tok(sym.EOF,null);
%eofval}

STRING_TEXT=(\\\"|[^\n\"]|[\n\ \t\b\012]+\\)*
LETTER=[a-záàâãäåéèêëíìîïóòõôöøúùûüçæñß_]
DIGIT=[0-9]
ALPHANUM=({LETTER}|{DIGIT})

%%

ALL                     { return tok (sym.ALL,         null); }
AND			{ return tok (sym.AND,         null); }
AS			{ return tok (sym.AS,          null); }
BOOL                    { return tok (sym.BOOL,        null); }
BY			{ return tok (sym.BY,          null); }
CACHE                   { return tok (sym.CACHE,       null); }
CHARACTER               { return tok (sym.CHARACTER,   null); }
CHAR                    { return tok (sym.CHARACTER,   null); }
CHECK                   { return tok (sym.CHECK,       null); }
CONSTRAINT              { return tok (sym.CONSTRAINT,  null); }
CREATE                  { return tok (sym.CREATE,      null); }
CYCLE                   { return tok (sym.CYCLE,       null); }
DATE                    { return tok (sym.DATE,        null); }
DATETIME                { return tok (sym.DATETIME,    null); }
DEFAULT                 { return tok (sym.DEFAULT,     null); }
DOUBLE                  { return tok (sym.DOUBLE,      null); }
FLOAT4                  { return tok (sym.FLOAT4,      null); }
FLOAT8                  { return tok (sym.FLOAT8,      null); }
FOREIGN			{ return tok (sym.FOREIGN,     null); }
FROM                    { return tok (sym.FROM,        null); }
FUNCTION		{ return tok (sym.FUNCTION,    null); }
GRANT                   { return tok (sym.GRANT,       null); }
HANDLER			{ return tok (sym.HANDLER,     null); }
INCREMENT               { return tok (sym.INCREMENT,   null); }
INDEX                   { return tok (sym.INDEX,       null); }
INHERITS                { return tok (sym.INHERITS,    null); }
INT2                    { return tok (sym.INT2,        null); }
INT4                    { return tok (sym.INT4,        null); }
INTEGER                 { return tok (sym.INT4,        null); }
NUMERIC			{ return tok (sym.NUMERIC,     null); }
KEY			{ return tok (sym.KEY,         null); }
LANCOMPILER		{ return tok (sym.LANCOMPILER, null); }
LANGUAGE 		{ return tok (sym.LANGUAGE,    null); }
MAXVALUE                { return tok (sym.MAXVALUE,    null); }
MINVALUE                { return tok (sym.MINVALUE,    null); }
NOT                     { return tok (sym.NOT,         null); }
NULL                    { return tok (sym.NULL,        null); }
NUMBER			{ return tok (sym.NUMBER,      null); }
ON                      { return tok (sym.ON,          null); }
OPAQUE                  { return tok (sym.OPAQUE,      null); }
OR			{ return tok (sym.OR,          null); }
PRECISION               { return tok (sym.PRECISION,   null); }
PRIMARY			{ return tok (sym.PRIMARY,     null); }
PROCEDURAL		{ return tok (sym.PROCEDURAL,  null); }
PUBLIC                  { return tok (sym.PUBLIC,      null); }
REFERENCES		{ return tok (sym.REFERENCES,  null); }
RETURNS 		{ return tok (sym.RETURNS,     null); }
REVOKE                  { return tok (sym.REVOKE,      null); }
SELECT                  { return tok (sym.SELECT,      null); }
SEQUENCE                { return tok (sym.SEQUENCE,    null); }
SMALLINT                { return tok (sym.INT2,        null); }
START                   { return tok (sym.START,       null); }
TABLE                   { return tok (sym.TABLE,       null); }
TEXT                    { return tok (sym.TEXT,        null); }
TIME   			{ return tok (sym.TIME,        null); }
TIMESTAMP               { return tok (sym.TIMESTAMP,   null); }
TO                      { return tok (sym.TO,          null); }
TRUSTED 		{ return tok (sym.TRUSTED,     null); }
UNIQUE                  { return tok (sym.UNIQUE,      null); }
USING                   { return tok (sym.USING,       null); }
VARCHAR2                { return tok (sym.VARCHAR,     null); }
VARCHAR                 { return tok (sym.VARCHAR,     null); }
VARYING                 { return tok (sym.VARYING,     null); }
WITH   			{ return tok (sym.WITH,        null); }
ZONE   			{ return tok (sym.ZONE,        null); }
 
{LETTER}{ALPHANUM}*	{ return tok (sym.ID,     yytext()); }
{DIGIT}+		{ return tok (sym.NUMLIT, new Integer(yytext())); }
"("			{ return tok (sym.LPAR,   null); }
")"			{ return tok (sym.RPAR,   null); }
","			{ return tok (sym.COMMA,  null); }
";"			{ return tok (sym.SEMI,   null); }
"+"			{ return tok (sym.OPLUS,  null); }
"-"			{ return tok (sym.OMINUS, null); }
"*"			{ return tok (sym.OAST,   null); }
"/"			{ return tok (sym.OSLASH, null); }
"="|">"|"<"|">="|"<="	{ return tok (sym.OREL,   null); }

\"(\\\"|[^\n\"]|[\n\ \t\b\012]+\\)*\" {
  return tok (sym.STRING, yytext().substring(1,yytext().length() - 1));
}

\'(\\\'|\'\'|[^\'])*\' {
  return tok (sym.STRING, yytext().substring(1,yytext().length() - 1));
}

"--".*\n		{ }
"/*".*"*/"		{ }

\\CONNECT.*\n		{ }
"[ \t]"+		{ }
\n			{ }
.			{ }
