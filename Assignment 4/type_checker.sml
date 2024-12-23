(* use "symbol_table.sml"; *)

val staticstack = []
val dynamicstack = []
exception Error of string;

fun checkVarDecls(Datatypes.VarDecls([]), typenv, valenv, staticstack) = (typenv, valenv)
    | checkVarDecls(Datatypes.VarDecls((variable, varType)::rest), typenv, valenv, staticstack) =
        let
           val (newtypenv, newvalenv) = SymbolTable.add_var(variable, varType, typenv, valenv)
        in checkVarDecls(Datatypes.VarDecls(rest), newtypenv, newvalenv, staticstack) 
        end

and checkProcDecls(Datatypes.ProcDecls([]), typenv, valenv, staticstack) = (typenv, valenv)
    | checkProcDecls(Datatypes.ProcDecls((Datatypes.ProcDef(variable, proc_ast))::rest), typenv, valenv, staticstack) =
        let val (newtypenv, newvalenv) = SymbolTable.add_var(variable, Datatypes.procedure, typenv, valenv)
            val typcheck = checkBlock(proc_ast, (newtypenv, newvalenv)::staticstack)
            val addvalenv = SymbolTable.change_value(variable, newvalenv, SOME (Datatypes.valProc(proc_ast)))
        in checkProcDecls(Datatypes.ProcDecls(rest), newtypenv, addvalenv, staticstack)
        end

and checkDeclSeq(Datatypes.DeclSeq(varDecls, procDecls), staticstack) = 
    let val (emptytypenv, emptyvalenv) = SymbolTable.constructor()
        val (typenv, valenv) = checkVarDecls(varDecls, emptytypenv, emptyvalenv, staticstack)
        val (finaltypenv, finalvalenv) = checkProcDecls(procDecls, typenv, valenv, staticstack)
    in (finaltypenv, finalvalenv)::staticstack
    end

and checkIdentType(ident, []) = (print("variable "^ident^" not found");raise Error "checkIdentType: variable not found")
    | checkIdentType(ident, staticstack) =
        let val (typenv, valenv) = hd(staticstack)
            val returnval = SymbolTable.find_var(ident, typenv, valenv)
        in case returnval of SOME (identtype, value) => identtype
            | NONE => checkIdentType(ident, tl(staticstack))
        end

and checkExprType(Datatypes.Binop(expression1, Datatypes.RatAdd, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.rational
            else raise Error "RatAdd: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.RatSub, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.rational
            else raise Error "RatSub: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.RatMul, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.rational
            else raise Error "RatMul: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.RatDiv, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.rational
            else raise Error "RatDiv: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.IntAdd, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.integer
            else raise Error "IntAdd: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.IntSub, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.integer
            else raise Error "IntSub: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.IntMul, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.integer
            else raise Error "IntMul: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.IntDiv, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.integer
            else raise Error "IntDiv: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.IntMod, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.integer
            else raise Error "IntMod: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.And, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.boolean) andalso (expr2type = Datatypes.boolean) then Datatypes.boolean
            else raise Error "And: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.Or, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.boolean) andalso (expr2type = Datatypes.boolean) then Datatypes.boolean
            else raise Error "Or: type mismatch")
        end
    | checkExprType(Datatypes.Relop(expression1, Datatypes.Equal, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.boolean
            else if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.boolean
            else if ((expr1type = Datatypes.boolean) andalso (expr2type = Datatypes.boolean)) then Datatypes.boolean
            else raise Error "Relop: type mismatch")
        end
    | checkExprType(Datatypes.Relop(expression1, Datatypes.NotEqual, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.boolean
            else if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.boolean
            else if ((expr1type = Datatypes.boolean) andalso (expr2type = Datatypes.boolean)) then Datatypes.boolean
            else raise Error "Relop: type mismatch")
        end
    | checkExprType(Datatypes.Relop(expression1, relop, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.boolean
            else if ((expr1type = Datatypes.rational) andalso (expr2type = Datatypes.rational)) then Datatypes.boolean
            else raise Error "Relop: type mismatch")
        end
    | checkExprType(Datatypes.Uniop(Datatypes.Negate, expression), staticstack) =
        let val exprtype = checkExprType(expression, staticstack)
        in (if (exprtype = Datatypes.integer) then Datatypes.integer
            else if (exprtype = Datatypes.rational) then Datatypes.rational
            else raise Error "Negate: type mismatch")
        end
    | checkExprType(Datatypes.Uniop(Datatypes.Inverse, expression), staticstack) =
        let val exprtype = checkExprType(expression, staticstack)
        in (if (exprtype = Datatypes.rational) then Datatypes.rational
            else raise Error "Inverse: type mismatch")
        end
    | checkExprType(Datatypes.Uniop(Datatypes.Not, expression), staticstack) =
        let val exprtype = checkExprType(expression, staticstack)
        in (if (exprtype = Datatypes.boolean) then Datatypes.boolean
            else raise Error "Not: type mismatch")
        end
    | checkExprType(Datatypes.Binop(expression1, Datatypes.MakeRat, expression2), staticstack) =
        let val expr1type = checkExprType(expression1, staticstack)
            val expr2type = checkExprType(expression2, staticstack)
        in (if (expr1type = Datatypes.integer) andalso (expr2type = Datatypes.integer) then Datatypes.rational
            else raise Error "MakeRat: type mismatch")
        end
    | checkExprType(Datatypes.Uniop(Datatypes.ToRat, expression), staticstack) =
        let val exprtype = checkExprType(expression, staticstack)
        in (if (exprtype = Datatypes.integer) then Datatypes.rational
            else raise Error "ToRat: type mismatch")
        end
    | checkExprType(Datatypes.Int(intconst), staticstack) = Datatypes.integer
    | checkExprType(Datatypes.Rat(realconst), staticstack) = Datatypes.rational
    | checkExprType(Datatypes.Bool(boolconst), staticstack) = Datatypes.boolean
    | checkExprType(Datatypes.Ident(ident), staticstack) = checkIdentType(ident, staticstack)

and checkCom (Datatypes.AssignmentCmd(ident, expression), staticstack) = 
        let val exprtype = checkExprType(expression, staticstack)
            val identtype = checkIdentType(ident, staticstack)
        in if exprtype = identtype then true else raise Error "AssignmentCmd: type mismatch"
        end
    | checkCom (Datatypes.CallCmd(ident), staticstack) = 
        let val identtype = checkIdentType(ident, staticstack)
        in if identtype = Datatypes.procedure then true else raise Error "CallCmd: type mismatch"
        end
    | checkCom (Datatypes.ReadCmd(ident), staticstack) = 
        let val checkident = checkIdentType(ident, staticstack)
        in true 
        end
    | checkCom (Datatypes.PrintCmd(expression), staticstack) = 
        let val checkexpr = checkExprType(expression, staticstack)
        in true
        end
    | checkCom (Datatypes.ConditionalCmd(expression, comSeq1, comSeq2), staticstack) = 
        let val exprtype = checkExprType(expression, staticstack)
            val comSeq1type = checkComSeq(comSeq1, staticstack)
            val comSeq2type = checkComSeq(comSeq2, staticstack)
        in if (exprtype = Datatypes.boolean) andalso (comSeq1type = true) andalso (comSeq2type = true) 
            then true else raise Error "IfCmd: type mismatch"
        end
    | checkCom (Datatypes.WhileCmd(expression, comSeq), staticstack) = 
        let val exprtype = checkExprType(expression, staticstack)
            val comSeqtype = checkComSeq(comSeq, staticstack)
        in if (exprtype = Datatypes.boolean) andalso (comSeqtype = true) 
            then true else raise Error "WhileCmd: type mismatch"
        end

and checkComSeq(Datatypes.CommandSeq([]), staticstack) = true
    | checkComSeq(Datatypes.CommandSeq(com::rest), staticstack) = 
        let val _ = checkCom(com, staticstack)
        in checkComSeq(Datatypes.CommandSeq(rest), staticstack)
        end

and checkBlock(Datatypes.Block(declSeq, comSeq), staticstack) = 
    let val newstaticstack = checkDeclSeq(declSeq, staticstack)
    in (checkComSeq(comSeq, newstaticstack), newstaticstack)
    end

and checkProgram(program) = checkBlock(program, [])

