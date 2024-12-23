signature pl0_TOKENS =
sig
type ('a,'b) token
type svalue
val IDENT: (Datatypes.ident) *  'a * 'a -> (svalue,'a) token
val BOOLEAN: (bool) *  'a * 'a -> (svalue,'a) token
val INTEGER: (BigInt.bigint) *  'a * 'a -> (svalue,'a) token
val RATIONAL: (Rational.rational) *  'a * 'a -> (svalue,'a) token
val FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val MAKERAT:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val BOOLVAR:  'a * 'a -> (svalue,'a) token
val INTVAR:  'a * 'a -> (svalue,'a) token
val RATVAR:  'a * 'a -> (svalue,'a) token
val BOOLNOT:  'a * 'a -> (svalue,'a) token
val BOOLOR:  'a * 'a -> (svalue,'a) token
val BOOLAND:  'a * 'a -> (svalue,'a) token
val GREATEREQUAL:  'a * 'a -> (svalue,'a) token
val LESSEQUAL:  'a * 'a -> (svalue,'a) token
val GREATER:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val NOTEQUAL:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val INTMOD:  'a * 'a -> (svalue,'a) token
val INTDIV:  'a * 'a -> (svalue,'a) token
val INTMUL:  'a * 'a -> (svalue,'a) token
val INTSUB:  'a * 'a -> (svalue,'a) token
val INTPLUS:  'a * 'a -> (svalue,'a) token
val RATDIV:  'a * 'a -> (svalue,'a) token
val RATMUL:  'a * 'a -> (svalue,'a) token
val RATSUB:  'a * 'a -> (svalue,'a) token
val RATPLUS:  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature pl0_LRVALS=
sig
structure Tokens : pl0_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
