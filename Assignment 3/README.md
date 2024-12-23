<center>

# Garv Nagori

## 2021CS10549
</center>

### Grammar for Rational Number

This grammar is defined for rational numbers in EBNF syntax:

    Rational ::= Fraction | Decimal
    Fraction ::= [Sign] Digit {Digit} "/" NonZero {Digit}
    Decimal ::= [Sign] {Digit} "." {Digit} "(" Digit {Digit} ")"
    Sign ::= "+" | "~"
    NonZero ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    Digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

### Grammar for Rational Number Expressions

This grammar is defined for rational number expressions over the above grammar in EBNF syntax:

    Exp ::= Exp "+" Term | Exp "-" Term | Term
    Term ::= Term "*" Field | Term "/" Field | Field
    Field ::= Variable | Rational | Bigint
    Variable ::= Letter {Letter | Digit}
    Bigint ::= Digit {Digit}
    Letter ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" |"R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" |"i" | "j" |"k" | "l" |"m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
    Digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

Rational is defined as above