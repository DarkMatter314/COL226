(* pi.lex *)
structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
(fileName,bad,line,col) => TextIO.output(TextIO.stdOut,fileName^"[" ^Int.toString line^"."^Int.toString col^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

%%
%full
%header (functor pl0LexFun(structure Tokens: pl0_TOKENS));
%arg (fileName:string);
%s PI COMMENT;
alpha = [A-Za-z];
hexa = "0"("x"|"X")[0-9A-Fa-f];
digit = [0-9];
sign = [~];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0; YYBEGIN PI; continue ());
<PI>{ws}* => (continue ());
<PI>{eol} => (lin:=(!lin)+1; eolpos:=yypos+size yytext; continue ());
<PI>"(*" => (YYBEGIN COMMENT; continue ());
<PI>"~" => (col:=yypos-(!eolpos); T.MINUS(!lin,!col));
<PI>"+" => (col:=yypos-(!eolpos); T.INTPLUS(!lin,!col));
<PI>"-" => (col:=yypos-(!eolpos); T.INTSUB(!lin,!col));
<PI>"*" => (col:=yypos-(!eolpos); T.INTMUL(!lin,!col));
<PI>"/" => (col:=yypos-(!eolpos); T.INTDIV(!lin,!col));
<PI>"%" => (col:=yypos-(!eolpos); T.INTMOD(!lin,!col));
<PI>"inverse" => (col:=yypos-(!eolpos); T.INVERSE(!lin,!col));
<PI>".+." => (col:=yypos-(!eolpos); T.RATPLUS(!lin,!col));
<PI>".-." => (col:=yypos-(!eolpos); T.RATSUB(!lin,!col));
<PI>".*." => (col:=yypos-(!eolpos); T.RATMUL(!lin,!col));
<PI>"./." => (col:=yypos-(!eolpos); T.RATDIV(!lin,!col));
<PI>"!" => (col:=yypos-(!eolpos); T.BOOLNOT(!lin,!col));
<PI>"&&" => (col:=yypos-(!eolpos); T.BOOLAND(!lin,!col));
<PI>"||" => (col:=yypos-(!eolpos); T.BOOLOR(!lin,!col));
<PI>"=" => (col:=yypos-(!eolpos); T.EQUAL(!lin,!col));
<PI>"<>" => (col:=yypos-(!eolpos); T.NOTEQUAL(!lin,!col));
<PI>"<" => (col:=yypos-(!eolpos); T.LESS(!lin,!col));
<PI>"<=" => (col:=yypos-(!eolpos); T.LESSEQUAL(!lin,!col));
<PI>">" => (col:=yypos-(!eolpos); T.GREATER(!lin,!col));
<PI>">=" => (col:=yypos-(!eolpos); T.GREATEREQUAL(!lin,!col));
<PI>":=" => (col:=yypos-(!eolpos); T.ASSIGN(!lin,!col));
<PI>"(" => (col:=yypos-(!eolpos); T.LPAREN(!lin,!col)); 
<PI>")" => (col:=yypos-(!eolpos); T.RPAREN(!lin,!col));
<PI>"{" => (col:=yypos-(!eolpos); T.LBRACE(!lin,!col));
<PI>"}" => (col:=yypos-(!eolpos); T.RBRACE(!lin,!col));
<PI>"," => (col:=yypos-(!eolpos); T.COMMA(!lin,!col));
<PI>";" => (col:=yypos-(!eolpos); T.SEMI(!lin,!col));
<PI>"call" => (col:=yypos-(!eolpos); T.CALL(!lin,!col));
<PI>"read" => (col:=yypos-(!eolpos); T.READ(!lin,!col));
<PI>"print" => (col:=yypos-(!eolpos); T.PRINT(!lin,!col));
<PI>"if" => (col:=yypos-(!eolpos); T.IF(!lin,!col));
<PI>"then" => (col:=yypos-(!eolpos); T.THEN(!lin,!col));
<PI>"else" => (col:=yypos-(!eolpos); T.ELSE(!lin,!col));
<PI>"fi" => (col:=yypos-(!eolpos); T.FI(!lin,!col));
<PI>"while" => (col:=yypos-(!eolpos); T.WHILE(!lin,!col));
<PI>"do" => (col:=yypos-(!eolpos); T.DO(!lin,!col));
<PI>"od" => (col:=yypos-(!eolpos); T.OD(!lin,!col));
<PI>"rational" => (col:=yypos-(!eolpos); T.RATVAR(!lin,!col));
<PI>"integer" => (col:=yypos-(!eolpos); T.INTVAR(!lin,!col));
<PI>"boolean" => (col:=yypos-(!eolpos); T.BOOLVAR(!lin,!col));
<PI>"procedure" => (col:=yypos-(!eolpos); T.PROCEDURE(!lin,!col));
<PI>"tt" => (col:=yypos-(!eolpos); T.BOOLEAN(true, !lin,!col));
<PI>"ff" => (col:=yypos-(!eolpos); T.BOOLEAN(false, !lin,!col));
<PI>"make_rat" => (col:=yypos-(!eolpos); T.MAKERAT(!lin,!col));
<PI>"rat" => (col:=yypos-(!eolpos); T.RAT(!lin,!col));
<PI>"fromDecimal" => (col:=yypos-(!eolpos); T.FROMDECIMAL(!lin,!col));
<PI>{sign}?({digit}*)"."({digit}*)"("({digit}+)")" => (col:=yypos-(!eolpos); T.RATIONAL(Rational.fromDecimal(yytext),!lin,!col));
<PI>{sign}?({digit}+) => (col:=yypos-(!eolpos); T.INTEGER(BigInt.toInt(yytext),!lin,!col));
<PI>{alpha}(({alpha}|{digit}))* => (col:=yypos-(!eolpos); T.IDENT(yytext,!lin,!col));
<PI>. => (col:=yypos-(!eolpos); badCh (fileName,yytext,!lin,!col); print("Ignoring bad character "^yytext^" at "^Int.toString(!lin)^":"^Int.toString(!col)^"\n"); continue());
<COMMENT>{eol} => (lin:=(!lin)+1;eolpos:=yypos+size yytext; continue ());
<COMMENT>"*)" => (YYBEGIN PI; continue ());
<COMMENT>. => (continue ());