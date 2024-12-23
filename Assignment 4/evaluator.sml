(* use "symbol_table.sml"; *)
exception Error of string;

fun evalVarDecls(Datatypes.VarDecls([]), typenv, valenv, staticstack) = (typenv, valenv)
    | evalVarDecls(Datatypes.VarDecls((variable, varType)::rest), typenv, valenv, staticstack) =
        let
           val (newtypenv, newvalenv) = SymbolTable.add_var(variable, varType, typenv, valenv)
        in evalVarDecls(Datatypes.VarDecls(rest), newtypenv, newvalenv, staticstack) 
        end

and evalProcDecls(Datatypes.ProcDecls([]), typenv, valenv, staticstack) = (typenv, valenv)
    | evalProcDecls(Datatypes.ProcDecls((Datatypes.ProcDef(variable, proc_ast))::rest), typenv, valenv, staticstack) =
        let val (newtypenv, newvalenv) = SymbolTable.add_var(variable, Datatypes.procedure, typenv, valenv)
            val addvalenv = SymbolTable.change_value(variable, newvalenv, SOME (Datatypes.valProc(proc_ast)))
        in evalProcDecls(Datatypes.ProcDecls(rest), newtypenv, addvalenv, staticstack)
        end

and evalDeclSeq(Datatypes.DeclSeq(varDecls, procDecls), staticstack) =
    let val (emptytypenv, emptyvalenv) = SymbolTable.constructor()
        val (typenv, valenv) = evalVarDecls(varDecls, emptytypenv, emptyvalenv, staticstack)
        val (finaltypenv, finalvalenv) = evalProcDecls(procDecls, typenv, valenv, staticstack)
    in (finaltypenv, finalvalenv)::staticstack
    end

and evalExp(Datatypes.Binop(expression1, Datatypes.RatAdd, expression2), staticstack, dynamicstack) =
        let val Datatypes.valRat(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valRat(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valRat(Rational.add(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.RatSub, expression2), staticstack, dynamicstack) =
        let val Datatypes.valRat(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valRat(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valRat(Rational.subtract(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.RatMul, expression2), staticstack, dynamicstack) =
        let val Datatypes.valRat(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valRat(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valRat(Rational.multiply(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.RatDiv, expression2), staticstack, dynamicstack) =
        let val Datatypes.valRat(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valRat(expr2) = evalExp(expression2, staticstack, dynamicstack)
            val ratopt = Rational.divide(expr1, expr2)
        in case ratopt of NONE => raise Error "Cannot divide by zero"
            | SOME ratval => Datatypes.valRat(ratval)
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.IntAdd, expression2), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valInt(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valInt(BigInt.add(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.IntSub, expression2), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valInt(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valInt(BigInt.sub(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.IntMul, expression2), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valInt(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valInt(BigInt.mul(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.IntDiv, expression2), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valInt(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valInt(BigInt.divide(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.IntMod, expression2), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valInt(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in Datatypes.valInt(BigInt.modulo(expr1, expr2))
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.And, expression2), staticstack, dynamicstack) =
        let val Datatypes.valBool(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valBool(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in if(expr1 andalso expr2) then Datatypes.valBool(true)
            else Datatypes.valBool(false)
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.Or, expression2), staticstack, dynamicstack) =
        let val Datatypes.valBool(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valBool(expr2) = evalExp(expression2, staticstack, dynamicstack)
        in if(expr1 orelse expr2) then Datatypes.valBool(true)
            else Datatypes.valBool(false)
        end
    | evalExp(Datatypes.Relop(expression1, Datatypes.Equal, expression2), staticstack, dynamicstack) =
        let val expr1 = evalExp(expression1, staticstack, dynamicstack)
            val expr2 = evalExp(expression2, staticstack, dynamicstack)
        in case expr1 of Datatypes.valInt(int1) =>
                (case expr2 of Datatypes.valInt(int2) => Datatypes.valBool(BigInt.isequal(int1, int2))
                            | other => raise Error "Type checking fail")
            | Datatypes.valRat(rat1) =>
                (case expr2 of Datatypes.valRat(rat2) => Datatypes.valBool(Rational.equal(rat1, rat2))
                            | other => raise Error "Type checking fail")
            | Datatypes.valBool(bool1) =>
                (case expr2 of Datatypes.valBool(bool2) => Datatypes.valBool(bool1 = bool2)
                            | other => raise Error "Type checking fail")
            | other => raise Error "Type checking fail" 
        end
    | evalExp(Datatypes.Relop(expression1, Datatypes.NotEqual, expression2), staticstack, dynamicstack) =
        let val expr1 = evalExp(expression1, staticstack, dynamicstack)
            val expr2 = evalExp(expression2, staticstack, dynamicstack)
        in case expr1 of Datatypes.valInt(int1) =>
                (case expr2 of Datatypes.valInt(int2) => Datatypes.valBool(not(BigInt.isequal(int1, int2)))
                            | other => raise Error "Type checking fail")
            | Datatypes.valRat(rat1) =>
                (case expr2 of Datatypes.valRat(rat2) => Datatypes.valBool(not(Rational.equal(rat1, rat2)))
                            | other => raise Error "Type checking fail")
            | Datatypes.valBool(bool1) =>
                (case expr2 of Datatypes.valBool(bool2) => Datatypes.valBool(bool1 <> bool2)
                            | other => raise Error "Type checking fail")
            | other => raise Error "Type checking fail" 
        end
    | evalExp(Datatypes.Relop(expression1, Datatypes.Less, expression2), staticstack, dynamicstack) =
        let val expr1 = evalExp(expression1, staticstack, dynamicstack)
            val expr2 = evalExp(expression2, staticstack, dynamicstack)
        in case expr1 of Datatypes.valInt(int1) =>
                (case expr2 of Datatypes.valInt(int2) => Datatypes.valBool(BigInt.isless(int1, int2))
                            | other => raise Error "Type checking fail")
            | Datatypes.valRat(rat1) =>
                (case expr2 of Datatypes.valRat(rat2) => Datatypes.valBool(Rational.less(rat1, rat2))
                            | other => raise Error "Type checking fail")
            | other => raise Error "Type checking fail" 
        end
    | evalExp(Datatypes.Relop(expression1, Datatypes.Greater, expression2), staticstack, dynamicstack) =
        let val expr1 = evalExp(expression1, staticstack, dynamicstack)
            val expr2 = evalExp(expression2, staticstack, dynamicstack)
        in case expr1 of Datatypes.valInt(int1) =>
                (case expr2 of Datatypes.valInt(int2) => Datatypes.valBool(BigInt.isless(int2, int1))
                            | other => raise Error "Type checking fail")
            | Datatypes.valRat(rat1) =>
                (case expr2 of Datatypes.valRat(rat2) => Datatypes.valBool(Rational.less(rat2, rat1))
                            | other => raise Error "Type checking fail")
            | other => raise Error "Type checking fail" 
        end
    | evalExp(Datatypes.Relop(expression1, Datatypes.LessEq, expression2), staticstack, dynamicstack) =
        let val expr1 = evalExp(expression1, staticstack, dynamicstack)
            val expr2 = evalExp(expression2, staticstack, dynamicstack)
        in case expr1 of Datatypes.valInt(int1) =>
                (case expr2 of Datatypes.valInt(int2) => Datatypes.valBool(BigInt.isless(int1, int2) orelse BigInt.isequal(int1, int2))
                            | other => raise Error "Type checking fail")
            | Datatypes.valRat(rat1) =>
                (case expr2 of Datatypes.valRat(rat2) => Datatypes.valBool(Rational.less(rat1, rat2) orelse Rational.equal(rat1, rat2))
                            | other => raise Error "Type checking fail")
            | other => raise Error "Type checking fail" 
        end
    | evalExp(Datatypes.Relop(expression1, Datatypes.GreaterEq, expression2), staticstack, dynamicstack) =
        let val expr1 = evalExp(expression1, staticstack, dynamicstack)
            val expr2 = evalExp(expression2, staticstack, dynamicstack)
        in case expr1 of Datatypes.valInt(int1) =>
                (case expr2 of Datatypes.valInt(int2) => Datatypes.valBool(BigInt.isless(int2, int1) orelse BigInt.isequal(int1, int2))
                            | other => raise Error "Type checking fail")
            | Datatypes.valRat(rat1) =>
                (case expr2 of Datatypes.valRat(rat2) => Datatypes.valBool(Rational.less(rat2, rat1) orelse Rational.equal(rat1, rat2))
                            | other => raise Error "Type checking fail")
            | other => raise Error "Type checking fail" 
        end
    | evalExp(Datatypes.Uniop(Datatypes.Negate, expression), staticstack, dynamicstack) =
        let val expr = evalExp(expression, staticstack, dynamicstack)
        in case expr of Datatypes.valInt(intval) => Datatypes.valInt(BigInt.negate(intval))
            | Datatypes.valRat(ratval) => Datatypes.valRat(Rational.neg(ratval))
            | other => raise Error "Type checking fail"
        end
    | evalExp(Datatypes.Uniop(Datatypes.Inverse, expression), staticstack, dynamicstack) =
        let val expr = evalExp(expression, staticstack, dynamicstack)
        in case expr of Datatypes.valRat(ratval) => 
            let val inverserat = Rational.inverse(ratval);
            in case inverserat of NONE => raise Error "Inverse cannot be applied to zero"
               | SOME inverseratval => Datatypes.valRat(inverseratval)
            end
            | other => raise Error "Type checking fail"
        end
    | evalExp(Datatypes.Uniop(Datatypes.Not, expression), staticstack, dynamicstack) =
        let val expr = evalExp(expression, staticstack, dynamicstack)
        in case expr of Datatypes.valBool(boolval) => Datatypes.valBool(not(boolval))
            | other => raise Error "Type checking fail"
        end
    | evalExp(Datatypes.Binop(expression1, Datatypes.MakeRat, expression2), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr1) = evalExp(expression1, staticstack, dynamicstack)
            val Datatypes.valInt(expr2) = evalExp(expression2, staticstack, dynamicstack)
            val expr = Rational.make_rat(expr1, expr2)
        in case expr of NONE => raise Error "Make rat fail"
            | SOME exprval => Datatypes.valRat(exprval)
        end
    | evalExp(Datatypes.Uniop(Datatypes.ToRat, expression), staticstack, dynamicstack) =
        let val Datatypes.valInt(expr) = evalExp(expression, staticstack, dynamicstack)
            val expr = Rational.rat(expr)
        in case expr of NONE => raise Error "To rat fail"
            | SOME exprval => Datatypes.valRat(exprval)
        end
    | evalExp(Datatypes.Int(intval), staticstack, dynamicstack) = Datatypes.valInt(intval)
    | evalExp(Datatypes.Rat(ratval), staticstack, dynamicstack) = Datatypes.valRat(ratval)
    | evalExp(Datatypes.Bool(boolval), staticstack, dynamicstack) = Datatypes.valBool(boolval)
    | evalExp(Datatypes.Ident(ident), staticstack, dynamicstack) = 
        let 
            fun identhelp (ident, [], tempstack) = raise Error "Identifier not found"
                | identhelp (ident, (typenv, valenv)::rest, tempstack) = 
                let val returnval = SymbolTable.find_var(ident, typenv, valenv)
                in case returnval of NONE => identhelp(ident, rest, (typenv, valenv)::tempstack)
                    | SOME (identtype, NONE) => raise Error "Identifier not initialised"
                    | SOME (identtype, SOME value) => value
                end
            
        in identhelp(ident, staticstack, [])
        end

and evalCom(Datatypes.AssignmentCmd(ident, expression), staticstack, dynamicstack, printfile) = 
    let val newvalue = evalExp(expression, staticstack, dynamicstack)
        val (typenv, valenv) = hd(staticstack)

        fun assignComHelp(ident, [], newval, tempstack) = raise Error "Identifier not found"
            | assignComHelp(ident, (typenv, valenv)::rest, newval, tempstack) =
            let val returnval = SymbolTable.find_var(ident, typenv, valenv)
            in case returnval of NONE => assignComHelp(ident, rest, newval, (typenv, valenv)::tempstack)
            | SOME (identtype, value) =>
                let val newvalhelp = SymbolTable.change_value(ident, valenv, SOME newval)
                in (List.rev(tempstack))@((typenv, newvalhelp)::rest)
                end
            end

    in (assignComHelp(ident, staticstack, newvalue, []), dynamicstack)
    end

    | evalCom(Datatypes.CallCmd(ident), staticstack, dynamicstack, printfile) =
        let fun findProc(ident, [], tempstack) = raise Error "Procedure not found"
                | findProc(ident, (typenv, valenv)::rest, tempstack) = 
                    let val returnval = SymbolTable.find_var(ident, typenv, valenv)
                    in case returnval of NONE => findProc(ident, rest, (typenv, valenv)::tempstack)
                        | SOME (Datatypes.procedure, NONE) => raise Error "Procedure not initialised"
                        | SOME (Datatypes.procedure, SOME (Datatypes.valProc(procval))) => (procval, tempstack, (typenv, valenv)::rest)
                        | _ => raise Error "Procedure not found" 
                    end

            val (procval, tempstack, procstaticstack) = findProc(ident, staticstack, [])
            val (newstaticstack, newdynamicstack) = evalBlock(procval, procstaticstack, dynamicstack, printfile)

        in (List.rev(tempstack)@newstaticstack, newdynamicstack)
        end

    | evalCom(Datatypes.ReadCmd(ident), staticstack, dynamicstack, printfile) =
        let fun dropWhile (f: 'a -> bool) (l: 'a list) =
                case l of
                    [] => []
                    | x :: xs => if f x then dropWhile (f) xs else l
        
            val inputText = valOf(TextIO.inputLine TextIO.stdIn)

            val inputval = implode(List.rev(dropWhile (fn x => x = #" " orelse x = #"\013" orelse x = #"\010") (tl(List.rev(explode(inputText))))))

            fun findident (ident, [], tempstack) = raise Error "Identifier not found"
                | findident (ident, (typenv, valenv)::rest, tempstack) = 
                let val returnval = SymbolTable.find_var(ident, typenv, valenv)
                in case returnval of NONE => findident(ident, rest, (typenv, valenv)::tempstack)
                    | SOME (identtype, value) => identtype
                end

            val identtype = findident(ident, staticstack, []) 

            fun changeVal (ident, [], newval, tempstack) = raise Error "Identifier not found"
                | changeVal (ident, (typenv, valenv)::rest, newval, tempstack) = 
                    let val returnval = SymbolTable.find_var(ident, typenv, valenv)
                    in case returnval of NONE => changeVal(ident, rest, newval, (typenv, valenv)::tempstack)
                        | SOME (identtype, value) =>
                            let val newvalhelp = SymbolTable.change_value(ident, valenv, newval)
                            in (List.rev(tempstack))@((typenv, newvalhelp)::rest)
                            end
                    end               

        in case identtype of 
            Datatypes.rational => 
                (let val inputRat = Rational.fromDecimal inputval
                in (changeVal(ident, staticstack, SOME (Datatypes.valRat(inputRat)), []), dynamicstack)
                end)
            | Datatypes.integer => 
                (let val inputInt = BigInt.toInt inputval
                in (changeVal(ident, staticstack, SOME (Datatypes.valInt(inputInt)), []), dynamicstack)
                end)
            | Datatypes.boolean => 
                (let val inputBool = if inputval = "tt" then true else false
                in (changeVal(ident, staticstack, SOME (Datatypes.valBool(inputBool)), []), dynamicstack)
                end)
            | _ => (raise Error "Cannot read procedure")
        end

    | evalCom(Datatypes.PrintCmd(expression), staticstack, dynamicstack, printfile) = 
        let  val value = evalExp(expression, staticstack, dynamicstack)
        in (case value of Datatypes.valInt(intval) => TextIO.output(printfile, (BigInt.showInt intval)^"\n")
            | Datatypes.valBool(boolval) => if boolval then TextIO.output(printfile,"tt\n") else TextIO.output(printfile, "ff\n")
            | Datatypes.valRat(ratval) => TextIO.output(printfile, (Rational.showDecimal ratval)^"\n")
            | Datatypes.valProc(procval) => raise Error "Cannot print procedure";
            (staticstack, dynamicstack))
        end

    | evalCom(Datatypes.ConditionalCmd(expression, comSeq1, comSeq2), staticstack, dynamicstack, printfile) =
        let val exprvalue = evalExp(expression, staticstack, dynamicstack)
        in if(exprvalue = Datatypes.valBool(true)) then evalComSeq(comSeq1, staticstack, dynamicstack, printfile)
            else evalComSeq(comSeq2, staticstack, dynamicstack, printfile)
        end

    | evalCom(Datatypes.WhileCmd(expression, comSeq), staticstack, dynamicstack, printfile) =
        let val exprvalue = evalExp(expression, staticstack, dynamicstack)
        in if(exprvalue = Datatypes.valBool(true)) then
                let val (newstaticstack, newdynamicstack) = evalComSeq(comSeq, staticstack, dynamicstack, printfile)
                in evalCom(Datatypes.WhileCmd(expression, comSeq), newstaticstack, newdynamicstack, printfile)
                end
            else (staticstack, dynamicstack)
        end
        

and evalComSeq(Datatypes.CommandSeq([]), staticstack, dynamicstack, printfile) = (staticstack, dynamicstack)
    | evalComSeq(Datatypes.CommandSeq(com::rest), staticstack, dynamicstack, printfile) = 
        let val (newstaticstack, newdynamicstack) = evalCom(com, staticstack, dynamicstack, printfile)
        in evalComSeq(Datatypes.CommandSeq(rest), newstaticstack, newdynamicstack, printfile)
        end

and evalBlock(Datatypes.Block(declSeq, comSeq), staticstack, dynamicstack, printfile) =
    let val newstaticstack = evalDeclSeq(declSeq, staticstack)
    in evalComSeq(comSeq, newstaticstack, dynamicstack, printfile)
    (* in (newstaticstack, dynamicstack) *)
    end

and evalProgram(program, printfile) = evalBlock(program, [], [], printfile)