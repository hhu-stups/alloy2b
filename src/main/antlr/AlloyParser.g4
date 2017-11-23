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

cmdDecl            : (cmdname=name COLON)? (RUN | CHECK) ( name | block ) scope;

scope              : FOR NUMBER ( EXPECT NUMBER )? // number permits more than just 0 or 1
                   | FOR NUMBER BUT typescope ( EXPECT NUMBER )? // was typescope,+ // number permits more than just 0 or 1
                   | typescope ( EXPECT NUMBER )? // was typescope,+ // number permits more than just 0 or 1
                   | ( EXPECT NUMBER )? ; // number permits more than just 0 or 1

typescope          : EXACTLY? NUMBER (name | INT | SEQ)? ;

// todo(see CstAstMapping.kt): there can be several names seperated by commas, smth like name (COMMA name)*
sigDecl            : sigQual* SIG name ( sigExt )? LBRACKET declList? RBRACKET ( block )? ; // was name,+

declList returns [ List decls ]
                   : vs+=decl (COMMA vs+=decl)* {
                       $decls = $vs;
                   } ;

enumDecl           : ENUM name LBRACKET name (COMMA name)* RBRACKET ;

sigQual            : ABSTRACT | LONE | ONE | SOME | PRIVATE ;

sigExt             : EXTENDS ref        # extendsExtension
                   | IN ref (PLUS ref)* # inExtension;

expr               : LET letDecl blockOrBar                # letExpr// was letDecl,+
                   | quant declList blockOrBar             # quantExpr
                   | unOp expr                             # unOpExpr
                   | left=expr binOp right=expr            # binOpExpr
                   | left=expr arrowOp right=expr          # arrowOpExpr
                   | left=expr NOT? compareOp right=expr   # compareExpr
                   | expr IMPLIES? expr ELSE expr          # ifExpr
                   | expr LSQBRACKET expr RSQBRACKET       # blockExpr // was expr,*
                   | NUMBER                                # numberExpr
                   | MINUS NUMBER                          # negNumberExpr
                   | NONE                                  # noneExpr
                   | IDEN                                  # idenExpr
                   | UNIV                                  # univExpr
                   | CAPINT                                # capIntExpr
                   | SeqInt                                # seqIntExpr
                   | LPAREN expr RPAREN                    # parenExpr
                   | AT? name                              # idExpr
                   | block                                 # blockExpr
                   | LBRACKET declList blockOrBar RBRACKET # declListExpr
                   ;

decl               : PRIVATE? DISJ? name COLON DISJ? expr ; //was name,+

letDecl            : name EQUAL expr ;

quant              : ALL | NO | SOME | LONE | ONE | SUM ;

binOp              : OR | AND | IFF | IMPLIES | PLUS | MINUS | DOT | INTERSECTION | UNION | DIFFERENCE |
                     DOM_RESTR | RAN_RESTR | OVERRIDE ; // todo: add | "<<" | ">>" | ">>>" ;

arrowOp            : ( SOME | ONE | LONE | SET )? ARROW ( SOME | ONE | LONE | SET )? ;

compareOp          : EQUAL | IN | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ;

unOp               : NOT | NO | SOME | LONE | ONE | SET | SEQ | ITERATION | CLOSURE | INVERSE | CARD;

block              : LBRACKET expr* RBRACKET ;

blockOrBar         : block     # blockInBlockOrBar
                   | DASH expr # exprInBlockOrBar;

name               : ID ; // was (THIS | ID) ( SLASH ID )* ;

ref                : name # nameRef
                   | UNIV # univRef
                   | CAPINT # capIntRef
                   | SeqInt # seqIntRef;