(* glue.sml Create a lexer and a parser *)
structure pl0LrVals = pl0LrValsFun(
                    structure Token = LrParser.Token);
structure pl0Lex = pl0LexFun(
                    structure Tokens = pl0LrVals.Tokens);
structure pl0Parser = JoinWithArg(
                    structure ParserData = pl0LrVals.ParserData
                    structure Lex=pl0Lex
                    structure LrParser=LrParser);