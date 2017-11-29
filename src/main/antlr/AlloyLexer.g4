lexer grammar AlloyLexer;

// whitespace
NEWLINE            : ('\r\n' | 'r' | '\n') -> skip ;
WS                 : [\t ]+ -> skip ;
COMMENT            : ('//' ~( '\r' | '\n' )*
                   | '/*' .*? '*/')
                   -> skip ;

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
THIS               : 'this' ;
SET                : 'set' ;
SUM                : 'sum' ;
IDEN               : 'iden' ;
UNIV               : 'univ' ;
ELSE               : 'else' ;
DISJ               : 'DISJ' ;

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
NONE               : 'none' ;

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
SLASH              : '/' ;
AT                 : '@' ;

// types
INT                : 'int' ;
CAPINT             : 'Int' ;
SeqInt             : 'Seq/Int' ;
SEQ                : 'seq' ;

// operators
AND                : '&&' | 'and' ;
IMPLIES            : 'implies' | '=>' ;
IFF                : '<=>' | 'iff' ;
OR                 : '||' | 'or' ;
NOT                : '!' | 'not' ;
ARROW              : '->' ;
EQUAL              : '=' ;
DOM_RESTR          : '<:' ;
RAN_RESTR          : ':>' ;
OVERRIDE           : '++' ;
PLUS               : '+' ;
MINUS              : '-' ;
IN                 : 'in' ;
ITERATION          : '*' ;
CLOSURE            : '^' ;
INVERSE            : '~' ;
CARD               : '#' ;
INTERSECTION       : '&' ;
UNION              : '+' ;
DIFFERENCE         : '-' ;
GREATER            : '>' ;
GREATER_EQUAL      : '>=' ;
LESS               : '<' ;
LESS_EQUAL         : '=<' ;

// identifiers
ID                 : [A-Za-z][A-Za-z0-9_]* ;

NUMBER             : [1-9][0-9]* ;