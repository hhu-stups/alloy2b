parser grammar AlloyParser;

options { tokenVocab=AlloyLexer; }

specification      : module? open* paragraph* ;

module             : MODULE name ; // was: name ( LSQBRACKET  EXACTLY? name  (COMMA EXACTLY? NUMBER)* RSQBRACKET )? ;

open               : PRIVATE? OPEN name ( LSQBRACKET ref RSQBRACKET )? ( AS name )? ; // was ref,+

paragraph          : factDecl | assertDecl | funDecl | predDecl | cmdDecl | enumDecl | sigDecl ;

factDecl           : FACT (name)? block ;

assertDecl         : ASSERT (name)? block ;

funDecl            : PRIVATE? FUN (ref DOT)? name LPAREN declList? RPAREN COLON expr block
                   | PRIVATE? FUN (ref DOT)? name LSQBRACKET declList? RSQBRACKET COLON expr block
                   | PRIVATE? FUN (ref DOT)? name COLON expr block ;

predDecl           : PRIVATE? PRED (ref DOT)? name LPAREN declList? RPAREN block
                   | PRIVATE? PRED (ref DOT)? name LSQBRACKET declList? RSQBRACKET block
                   | PRIVATE? PRED (ref DOT)? name block ;

cmdDecl            : (cmdname=name COLON)? (RUN | CHECK) ( name | block ) scope;

scope              : FOR NUMBER ( EXPECT NUMBER )?
                   | FOR NUMBER BUT typescope (COMMA typescope)* ( EXPECT NUMBER )?
                   | FOR typescope (COMMA typescope)* ( EXPECT NUMBER )?
                   | ( EXPECT NUMBER )? ; // number after expect permits more than just 0 or 1
                   // expect ... is alloy 3 syntax anyway (?)

typescope          : EXACTLY? NUMBER (name | INT | SEQ)? ;

sigDecl            : sigQual* SIG name (COMMA name)* ( sigExt )? LBRACKET declList? RBRACKET ( block )? ; // was name,+

declList           : decl (COMMA decl)* ;

exprList           : expr (COMMA expr)* ;

enumDecl           : ENUM name LBRACKET name (COMMA name)* RBRACKET ;

sigQual            : ABSTRACT | LONE | ONE | SOME | PRIVATE ;

sigExt             : EXTENDS ref        # extendsExtension
                   | IN ref (PLUS ref)* # inExtension;

expr               : LET letDecl (COMMA letDecl)* blockOrBar               # letExpr
                   | quant declList blockOrBar                             # quantExpr
                   | unOp expr                                             # unOpExpr
                   | left=expr DOT right=expr                              # dotJoinExpr
                   | left=expr binOp right=expr                            # binOpExpr
                   |<assoc=right> left=expr IMPLIES right=expr             # impliesExpr // needed because associativity differs
                   | left=expr arrowOp right=expr                          # arrowOpExpr
                   | left=expr NOT? compareOp right=expr                   # compareExpr
                   | exprQuantifier expr                                   # quantifiedExpr
                   | ifExpr=expr IMPLIES? thenExpr=expr ELSE elseExpr=expr # ifExpr
                   | expr LSQBRACKET exprList RSQBRACKET                   # boxJoinExpr
                   | NUMBER                                                # numberExpr
                   | MINUS NUMBER                                          # negNumberExpr
                   | NONE                                                  # noneExpr
                   | IDEN                                                  # idenExpr
                   | UNIV                                                  # univExpr
                   | CAPINT                                                # capIntExpr
                   | SeqInt                                                # seqIntExpr
                   | LPAREN expr RPAREN                                    # parenExpr
                   | AT? name                                              # idExpr
                   | block                                                 # blockExpr
                   | LBRACKET declList blockOrBar RBRACKET                 # declListExpr
                   ;

decl               : PRIVATE? DISJ? name (COMMA name)* COLON DISJ? expr ;

letDecl            : name EQUAL expr ;

quant              : ALL | NO | SOME | LONE | ONE | SUM ;

binOp              : OR | AND | IFF | PLUS | MINUS | INTERSECTION | UNION | DIFFERENCE |
                     DOM_RESTR | RAN_RESTR | OVERRIDE ; // todo: add | "<<" | ">>" | ">>>" ;  // todo: precedence

arrowOp            : ( SOME | ONE | LONE | SET )? ARROW ( SOME | ONE | LONE | SET )? ;

compareOp          : EQUAL | IN | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ;

unOp               : NOT | SEQ | ITERATION | CLOSURE | INVERSE | CARD;

exprQuantifier     : NO | SOME | LONE | ONE | SET;

block              : LBRACKET expr* RBRACKET ;

blockOrBar         : block     # blockInBlockOrBar
                   | DASH expr # exprInBlockOrBar;

name               : (THIS | ID) ( SLASH ID )* ;

ref                : name # nameRef
                   | UNIV # univRef
                   | CAPINT # capIntRef
                   | SeqInt # seqIntRef;