CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "rational.sml";
use "calc.grm.sig";
use "calc.grm.sml";
use "calc.lex.sml";
use "glue.sml";
fun invoke lexstream =
    let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut,
                          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in CalcParser.parse(0,lexstream,print_error,())
    end

fun parse () = 
    let val lexer = CalcParser.makeLexer
                      (fn _ => valOf(TextIO.inputLine TextIO.stdIn))
        val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
        val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = CalcParser.Stream.get lexer
             in case result
                  of SOME r =>
                      TextIO.output(TextIO.stdOut,
                             "result = " ^ (Rational.showRat r) ^ "\n")
                   | NONE => ();
                if CalcParser.sameToken(nextToken,dummyEOF) then ()
                else loop lexer
            end
     in loop lexer
    end