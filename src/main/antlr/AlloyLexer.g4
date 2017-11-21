lexer grammar AlloyLexer;

// whitespace
NEWLINE            : '\r\n' | 'r' | '\n' ;
WS                 : [\t ]+ ;

// keywords
SIG                : 'sig' ;
MODULE             : 'module' ;
PRIVATE            : 'private' ;
OPEN               : 'open' ;
AS                 : 'as' ;
FACT               : 'fact' ;

// quantifiers
EXACTLY            : 'exactly' ;

// brackets
LBRACKET           : '{' ;
RBRACKET           : '}' ;
LSQBRACKET         : '[' ;
RSQBRACKET         : ']' ;
LPAREN             : '(' ;
RPAREN             : ')' ;

// symbols
COMMA              : ',' ;
DASH               : '|' ;

// types
INT                : 'int' ;
CAPINT             : 'Int' ;
SeqInt             : 'Seq/Int' ;
SEQ                : 'seq' ;

// operators
AND                : '&&' | 'and' | '&' ;
IMPLIES            : 'implies' | '=>' ;
IFF                : '<=>' | 'iff' ;
OR                 : '||' | 'or' ;
NOT                : '!' | 'not' ;

// identifiers
ID                 : [A-Za-z][A-Za-z0-9_]* ;

NUMBER             : [1-9][0-9]*;