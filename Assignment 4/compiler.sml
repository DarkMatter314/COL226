CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "bigint.sml";
use "rational.sml";
use "datatypes.sml";
use "pl0.grm.sig";
use "pl0.grm.sml";
use "pl0.lex.sml";
use "glue.sml";
use "symbol_table.sml";
use "type_checker.sml";
use "evaluator.sml";
(* compiler.sml *)
structure pl0 :
sig val compile : string -> Datatypes.block
end =
struct
exception pl0Error;
fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn 
            n =>if TextIO.endOfStream inStream
                then "" 
                else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
                (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = pl0Parser.parse (15,(pl0Parser.makeLexer grab fileName),printError,fileName)
            handle pl0Parser.ParseError => raise pl0Error;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in tree
    end
end;

fun interpret(filein, fileout) =
    let val ast = pl0.compile filein;
        val printfile = TextIO.openOut fileout;
        val (typecheck, typestack) = checkProgram(ast);
        val (valcheck, valstack) = evalProgram(ast, printfile);
    in TextIO.closeOut printfile
    end;

Control.Print.printLength := 10000; 
Control.Print.printDepth := 10000; 
Control.Print.stringDepth := 10000;