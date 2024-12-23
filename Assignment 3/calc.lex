structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              output(std_out,"line " ^ (makestring l) ^
                               ": " ^ e ^ "\n")
%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
sign=[+|~];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{sign}?({digit}+) => (Tokens.NUM(valOf(Rational.rat(BigInt.toInt(yytext))), !pos, !pos));
{sign}?({digit}*)"."({digit}*)"("({digit}+)")" => (Tokens.NUM(Rational.fromDecimal(yytext), !pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
.        => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());