(* use "bigint.sml";
use "rational.sml";
use "datatypes.sml"; *)

signature SYMBOLTABLE =
sig
    type typtable
    type valtable
    exception environment_error of string
    val constructor : unit -> typtable * valtable
    val add_var : Datatypes.ident * Datatypes.basictype * typtable * valtable -> typtable * valtable
    val find_var : Datatypes.ident * typtable * valtable -> (Datatypes.basictype * Datatypes.value option) option
    val change_value : Datatypes.ident * valtable * Datatypes.value option -> valtable
end;

structure SymbolTable : SYMBOLTABLE =
struct

    type typtable = (Datatypes.ident * Datatypes.basictype) list;
    type valtable = (Datatypes.ident * (Datatypes.value option)) list;
    exception environment_error of string;

    fun constructor() = ([], [])

    fun add_var (variable: Datatypes.ident, vartype: Datatypes.basictype, typenv: typtable, valenv : valtable) =
        let
            val new_typenv = (variable, vartype)::typenv
            val new_valenv = (variable, NONE)::valenv
        in
            (new_typenv, new_valenv)
        end

    fun find_var (variable : Datatypes.ident, [], []) = NONE
        | find_var (variable : Datatypes.ident, (var, value)::rest, []) = raise environment_error("Value environment is empty")
        | find_var (variable : Datatypes.ident, [], (var, value)::rest) = raise environment_error("Type environment is empty")
        | find_var (variable : Datatypes.ident, (var, valtyp)::rest, (var2, value)::rest2) = 
            if (var <> var2) then raise environment_error("Type and Value environment mismatch") else
            if var = variable then SOME (valtyp, value) else find_var (variable, rest, rest2)

    fun change_value (variable : Datatypes.ident, valenv : valtable, newval : Datatypes.value option) =
        let
            fun change_help(variable, [], _, newval) = raise environment_error ("Value environment is empty")
            | change_help(variable, (var, value)::rest, helplist, newval) =
                if var = variable then List.rev(helplist) @ ((var, newval) :: rest)
                else change_help(variable, rest, (var, value)::helplist, newval)
        in
            change_help (variable, valenv, [], newval)
        end

end;