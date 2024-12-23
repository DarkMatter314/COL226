<center>

# Garv Nagori

## 2021CS10549
</center>

### Instructions to run

1. Unzip all the files present in the zip folder
2. Open terminal
3. Run the following commands one after the other
    
        sml compiler.sml
        interpret("<input_file>.rat", "<output_file>");
where the input file must have an extension of ```rat```

### Grammar for ```pl0+Rationals``` Language

The below grammar is defined for the programming language ```pl0+Rationals``` created using ```sml```, ```ml-lex``` and ```ml-yacc```

The terminals are given by 

    EOF | LBRACE| RBRACE | SEMI | COMMA | LPAREN |
    RPAREN | ASSIGN | CALL | READ | PRINT | IF | THEN | 
    ELSE | FI | WHILE | DO | OD | MINUS | INVERSE | 
    RATPLUS | RATSUB | RATMUL | RATDIV | INTPLUS | 
    INTSUB | INTMUL | INTDIV | INTMOD | EQUAL | 
    NOTEQUAL | RAT |LESS | GREATER | LESSEQUAL | 
    GREATEREQUAL | BOOLAND | BOOLOR | BOOLNOT | RATVAR | 
    INTVAR | BOOLVAR | PROCEDURE | MAKERAT | FROMDECIMAL |
    RATIONAL of Rational.rational | 
    INTEGER of BigInt.bigint | 
    BOOLEAN of bool | 
    IDENT of Datatypes.ident

and the non-terminals are given by 

    Program | Block | DeclSeq | VarDecls | RatVarDecl | IntVarDecl |
    BoolVarDecl | IdentList | ProcDecls | ProcDef | ComSeq | 
    Commands | Command | AssignCmd | CallCmd | ReadCmd | PrintCmd |
    IfCmd | WhileCmd | Expression

The grammar in EBNF syntax is:

    Program ::= Block
    Block ::= DeclSeq ComSeq
    DeclSeq ::= VarDecls ProcDecls
    VarDecls ::= RatVarDecl IntVarDecl BoolVarDecl
    RatVarDecl ::= RATVAR IDENT IdentList SEMI | ε
    IntVarDecl ::= INTVAR IDENT IdentList SEMI | ε
    BoolVarDecl ::= BOOLVAR IDENT IdentList SEMI | ε
    IdentList ::= COMMA IDENT IdentList | ε
    ProcDecls ::= ProcDef ProcDecls | ε
    ProcDef ::= PROCEDURE IDENT Block SEMI
    ComSeq ::= LBRACE Commands RBRACE | ε
    Commands ::= Command SEMI Commands | ε
    Command ::= AssignCmd
             | CallCmd
             | ReadCmd
             | PrintCmd
             | IfCmd
             | WhileCmd
    AssignCmd ::= IDENT ASSIGN Expression
    CallCmd ::= CALL IDENT
    ReadCmd ::= READ LPAREN IDENT RPAREN
    PrintCmd ::= PRINT LPAREN Expression RPAREN
    IfCmd ::= IF Expression THEN ComSeq ELSE ComSeq FI
    WhileCmd ::= WHILE Expression DO ComSeq OD
    Expression ::= RATIONAL
                | INTEGER
                | BOOLEAN
                | IDENT
                | Expression RATPLUS Expression
                | Expression RATSUB Expression
                | Expression RATMUL Expression
                | Expression RATDIV Expression
                | Expression INTPLUS Expression
                | Expression INTSUB Expression
                | Expression INTMUL Expression
                | Expression INTDIV Expression
                | Expression INTMOD Expression
                | Expression BOOLAND Expression
                | Expression BOOLOR Expression
                | MINUS Expression 
                | INVERSE Expression 
                | BOOLNOT Expression 
                | Expression EQUAL Expression
                | Expression NOTEQUAL Expression
                | Expression LESS Expression
                | Expression GREATER Expression
                | Expression LESSEQUAL Expression
                | Expression GREATEREQUAL Expression
                | LPAREN Expression RPAREN
                | MAKERAT LPAREN Expression COMMA Expression RPAREN
                | RAT LPAREN Expression RPAREN
                | FROMDECIMAL LPAREN RATIONAL RPAREN

### Design Decisions

1. An ```AST``` is created by parsing the tokens created by the lexer.
2. Then ***type-checking*** has been done by parsing the ```AST```
3. Finally, the ```AST``` is evaluated. 
4. Static scoping has been implemented by using a stack of ```symbol tables```
5. Whenever a procedure is called, a new ```symbol table``` is pushed into a ```staticstack```.
6. When the procedure being called is in a different scope a ```temporary stack``` is created to store the dynamic links of the calling stack.
7. Procedures cannot be printed.