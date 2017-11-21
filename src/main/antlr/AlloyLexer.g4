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
FUN                : 'fun' ;
PRED               : 'pred' ;
EXPECT             : 'expect' ;
FOR                : 'for' ;
BUT                : 'but' ;
ENUM               : 'enum' ;
ABSTRACT           : 'abstract' ;
EXTENDS            : 'extends' ;
LET                : 'let' ;


// commands
RUN                : 'run' ;
CHECK              : 'check' ;
ASSERT             : 'assert' ;

// quantifiers
EXACTLY            : 'exactly' ;
LONE               : 'lone' ;
ONE                : 'one' ;
SOME               : 'some' ;
ALL                : 'all' ;
NO                 : 'no' ;

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
DOT                : '.' ;
COLON              : ':' ;

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