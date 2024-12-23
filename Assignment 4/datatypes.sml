structure Datatypes = struct

    type ident = string

    datatype basictype = rational | integer | boolean | procedure

    and block = Block of declarationSeq*commandSeq

    and declarationSeq =  DeclSeq of varDecls*procDecls

    and varDecls = VarDecls of (ident*basictype) list

    and procDecls =  ProcDecls of procDef list 

    and procDef = ProcDef of ident*block

    and commandSeq = CommandSeq of command list

    and command = AssignmentCmd of ident * expression
                | CallCmd of ident
                | ReadCmd of ident
                | PrintCmd of expression
                | ConditionalCmd of expression * commandSeq * commandSeq
                | WhileCmd of expression * commandSeq

    and expression = Binop of expression * binop * expression
                    | Uniop of uniop * expression
                    | Relop of expression * relop * expression
                    | Rat of Rational.rational
                    | Int of BigInt.bigint
                    | Bool of bool
                    | Ident of ident

    and binop = IntAdd | IntSub | IntMul | IntDiv | IntMod | RatAdd | RatSub | RatMul | RatDiv | And | Or | MakeRat

    and uniop = Negate | Inverse | Not | ToRat

    and relop = Equal | NotEqual | Less | Greater | LessEq | GreaterEq

    and value = valRat of Rational.rational | valInt of BigInt.bigint | valBool of bool | valProc of block 

end