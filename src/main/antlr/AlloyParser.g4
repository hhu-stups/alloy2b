parser grammar AlloyParser;

options { tokenVocab=AlloyLexer; }

specification      : module? open* paragraph* ;

module             : name ( LSQBRACKET  EXACTLY? name  (COMMA EXACTLY? NUMBER)* RSQBRACKET )? ;

open               : PRIVATE? OPEN name ( LSQBRACKET ref RSQBRACKET )? ( AS name )? ; // was ref,+

paragraph          : factDecl | assertDecl | funDecl | cmdDecl | enumDecl | sigDecl ;

factDecl           : FACT (name)? block ;

assertDecl         : ASSERT (name)? block ;

funDecl            : PRIVATE? FUN (ref DOT)? name LPAREN declList? RPAREN COLON expr block
                   | PRIVATE? FUN (ref DOT)? name LSQBRACKET declList? RSQBRACKET COLON expr block
                   | PRIVATE? FUN (ref DOT)? name COLON expr block
                   | PRIVATE? PRED (ref DOT)? name LPAREN declList? RPAREN block
                   | PRIVATE? PRED (ref DOT)? name LSQBRACKET declList? RSQBRACKET block
                   | PRIVATE? PRED (ref DOT)? name block ;

cmdDecl            : (name COLON)? (RUN | CHECK) ( name | block ) scope;

scope              : FOR NUMBER ( EXPECT NUMBER )? // number permits more than just 0 or 1
                   | FOR NUMBER BUT typescope ( EXPECT NUMBER )? // was typescope,+ // number permits more than just 0 or 1
                   | typescope ( EXPECT NUMBER )? // was typescope,+ // number permits more than just 0 or 1
                   | ( EXPECT NUMBER )? ; // number permits more than just 0 or 1

typescope          : EXACTLY? NUMBER (name | INT | SEQ)? ;

sigDecl            : sigQual* SIG name ( sigExt )? LBRACKET declList? RBRACKET ( block )? ; // was name,+

declList           : decl
                   | decl COMMA declList ;

enumDecl           : ENUM name LBRACKET name (COMMA name)* RBRACKET ;

sigQual            : ABSTRACT | LONE | ONE | SOME | PRIVATE ;

sigExt             : EXTENDS ref
                   | IN ref (PLUS ref)* ;

expr               : LET letDecl blockOrBar // was letDecl,+
                   | quant declList blockOrBar
                   | unOp expr
                   | expr binOp expr
                   | expr arrowOp expr
                   | expr NOT? compareOp expr
                   | expr IMPLIES? expr ELSE expr
                   | expr LSQBRACKET expr RSQBRACKET // was expr,*
                   | NUMBER
                   | MINUS NUMBER
                   | NONE
                   | IDEN
                   | UNIV
                   | CAPINT
                   | SeqInt
                   | LPAREN expr RPAREN
                   | AT? name
                   | block
                   | LBRACKET declList blockOrBar RBRACKET ;

decl               : PRIVATE? DISJ? name COLON DISJ? expr ; //was name,+

letDecl            : name EQUAL expr ;

quant              : ALL | NO | SOME | LONE | ONE | SUM ;

binOp              : OR | AND | IFF | IMPLIES | PLUS | MINUS | DOT; // todo: add | "++" | "<:" | ":>" | "<<" | ">>" | ">>>" ;

arrowOp            : ( SOME | ONE | LONE | SET )? ARROW ( SOME | ONE | LONE | SET )? ;

compareOp          : EQUAL | IN ; // todo: add | "<" | ">" | "=<" | ">=" ;

unOp               : NOT | NO | SOME | LONE | ONE | SET | SEQ | ITERATION | CLOSURE | INVERSE; // todo: add | "#" ;

block              : LBRACKET expr* RBRACKET ;

blockOrBar         : block
                   | DASH expr ;

name               : (THIS | ID) ( SLASH ID )* ;

ref                : name | UNIV | CAPINT | SeqInt ;