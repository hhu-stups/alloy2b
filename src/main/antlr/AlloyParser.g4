parser grammar AlloyParser;

options { tokenVocab=AlloyLexer; }

specification      : module? open* paragraph* ;

module             : MODULE name ( LSQBRACKET  EXACTLY? name  (COMMA EXACTLY? NUMBER)* RSQBRACKET )? ;

open               : PRIVATE? OPEN name ( LSQBRACKET ref (COMMA ref)* RSQBRACKET )? ( AS name )? ;

paragraph          : factDecl | assertDecl | funDecl | predDecl | runStatement | checkStatement | enumDecl | sigDecl ;

factDecl           : FACT (name)? block ;

assertDecl         : ASSERT (name)? block ;

funDecl            : PRIVATE? FUN (ref DOT)? name LPAREN declList? RPAREN COLON expr block
                   | PRIVATE? FUN (ref DOT)? name LSQBRACKET declList? RSQBRACKET COLON expr block
                   | PRIVATE? FUN (ref DOT)? name COLON expr block ;

predDecl           : PRIVATE? PRED (ref DOT)? name LPAREN declList? RPAREN block
                   | PRIVATE? PRED (ref DOT)? name LSQBRACKET declList? RSQBRACKET block
                   | PRIVATE? PRED (ref DOT)? name block ;

runStatement       : (cmdname=name COLON)? RUN ( name | block ) scope;

checkStatement     : (cmdname=name COLON)? CHECK ( name | block ) scope;

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

expr               : unOp expr                                                            # unOpExpr
                   | left=expr DOT right=expr                                             # dotJoinExpr
                   | expr LSQBRACKET exprList RSQBRACKET                                  # boxJoinExpr
                   | left=expr operator=restrOperator right=expr                          # restrictionOpExpr
                   | left=expr arrowOp right=expr                                         # arrowOpExpr
                   | left=expr INTERSECTION right=expr                                    # intersectionExpr
                   | left=expr OVERRIDE right=expr                                        # overrideExpr
                   | CARD expr                                                            # cardExpr
                   | left=expr operator=binOp right=expr                                  # binOpExpr
                                      | quant declList blockOrBar                                            # quantifierExpr

                   | exprQuantifier expr                                                  # quantifiedExpr
                   | left=expr NOT? compareOp right=expr                                  # compareExpr

                   | NOT expr                                                             # negatedExpr
                   | left=expr AND right=expr                                             # conjunctionExpr
                   |<assoc=right> ifExpr=expr IMPLIES thenExpr=expr (ELSE elseExpr=expr)? # impliesExpr // needed because associativity differs

                   | left=expr IFF right=expr                                             # equalityExpr
                   | left=expr OR right=expr                                              # disjunctionExpr

                   | LET letDecl (COMMA letDecl)* blockOrBar                              # letExpr

                   | INT LSQBRACKET expr RSQBRACKET                                       # intCastExpr
                   | INT LPAREN expr RPAREN                                               # intCastExpr
                   | INT expr                                                             # intCastExpr
                   | NUMBER                                                               # numberExpr
                   | MINUS NUMBER                                                         # negNumberExpr
                   | NONE                                                                 # noneExpr
                   | IDEN                                                                 # idenExpr
                   | UNIV                                                                 # univExpr
                   | CAPINT                                                               # capIntExpr
                   | SeqInt                                                               # seqIntExpr
                   | LPAREN expr RPAREN                                                   # parenExpr
                   | AT? name                                                             # idExpr
                   | block                                                                # blockExpr
                   | LBRACKET declList blockOrBar RBRACKET                                # declListExpr
                   ;

decl               : PRIVATE? DISJ? name (COMMA name)* COLON DISJ? expr ;

letDecl            : name EQUAL expr ;

quant              : ALL | NO | SOME | LONE | ONE | SUM ;

restrOperator      : DOM_RESTR | RAN_RESTR;

binOp              : PLUS | MINUS;

arrowOp            : ( SOME | ONE | LONE | SET )? ARROW ( SOME | ONE | LONE | SET )? ;

compareOp          : IN | GREATER_EQUAL | GREATER | LESS_EQUAL | LESS | EQUAL ;

unOp               : INVERSE | CLOSURE | ITERATION;

exprQuantifier     : NO | SOME | LONE | ONE | SET | SEQ;

block              : LBRACKET expr* RBRACKET ;

blockOrBar         : block     # blockInBlockOrBar
                   | DASH expr # exprInBlockOrBar;

name               : (THIS | ID) ( SLASH ID )* ;

ref                : name # nameRef
                   | UNIV # univRef
                   | CAPINT # capIntRef
                   | SeqInt # seqIntRef;