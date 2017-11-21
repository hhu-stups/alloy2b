lexer grammar AlloyLexer;

// Whitespace
NEWLINE            : '\r\n' | 'r' | '\n' ;
WS                 : [\t ]+ ;

// Signature Declaration
SIG                : 'sig' ;

// Brackets
LBRACKET           : '{' ;
RBRACKET           : '}' ;

// Identifiers
ID                 : [A-Za-z][A-Za-z0-9_]* ;