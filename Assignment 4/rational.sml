use "bigint.sml";

functor createRational(Bigint: BIGINT) :
    sig 
        type rational;
        exception rat_error;
        val make_rat: Bigint.bigint * Bigint.bigint -> rational option
        val rat: Bigint.bigint -> rational option
        val reci: Bigint.bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational option
        val equal : rational * rational -> bool (* equality *)
        val less : rational * rational -> bool (* less than *)
        val add : rational * rational -> rational (* addition *)
        val subtract : rational * rational -> rational (* subtraction *)
        val multiply : rational * rational -> rational (* multiplication *)
        val divide : rational * rational -> rational option (* division *)
        val showRat : rational -> string
        val showDecimal : rational -> string
        val fromDecimal : string -> rational
        val toDecimal : rational -> string
    end
=
    struct
    open Bigint;
    type rational = Bigint.bigint * Bigint.bigint;
    exception rat_error;

    fun make_rat (a, b) =
        if (Bigint.iszero(b)) then NONE 
        else
        let val gcd = Bigint.gcd(Bigint.abs(a), Bigint.abs(b));
        in 
            if(Bigint.isnegative(a) andalso Bigint.isnegative(b)) then 
                SOME (Bigint.divide(Bigint.abs(a), gcd), Bigint.divide(Bigint.abs(b), gcd))
            else if (Bigint.isnegative(a)) then 
                SOME (Bigint.divide(a, gcd), Bigint.divide(b, gcd))
            else if (Bigint.isnegative(b)) then 
                SOME (Bigint.divide(Bigint.negate(a), gcd), Bigint.divide(Bigint.negate(b), gcd))
            else SOME (Bigint.divide(a, gcd), Bigint.divide(b, gcd))
        end;

    fun rat (a) = make_rat (a, Bigint.unity);
    fun reci (a) = make_rat (Bigint.unity, a);
    fun neg ((a,b)) = (Bigint.negate(a), b);
    fun inverse (a, b) = make_rat(b,a)
    fun equal ((a,b),(c,d)) = Bigint.isequal(Bigint.mul(a,d), Bigint.mul(b,c))
    fun less ((a,b),(c,d)) = Bigint.isless(Bigint.mul(a,d), Bigint.mul(b,c))

    fun add ((a,b),(c,d)) = 
        let val added = make_rat(Bigint.add(Bigint.mul(a,d), Bigint.mul(b,c)), Bigint.mul(b,d))
        in case added  of NONE => raise rat_error
        | SOME x => x
        end

    fun subtract ((a,b),(c,d)) = 
        let val added = make_rat(Bigint.sub(Bigint.mul(a,d), Bigint.mul(b,c)), Bigint.mul(b,d))
        in case added  of NONE => raise rat_error
        | SOME x => x
        end

    fun multiply ((a,b),(c,d)) = 
        let val added = make_rat(Bigint.mul(a,c), Bigint.mul(b,d))
        in case added  of NONE => raise rat_error
        | SOME x => x
        end

    fun divide ((a,b),(c,d)) = make_rat(Bigint.mul(a,d), Bigint.mul(b,c))

    fun showRat ((a,b)) = Bigint.showInt(a)^"/"^Bigint.showInt(b)

    fun addZeroes (a, 0) = a
    | addZeroes (a, n) = addZeroes(a, n-1)^"0"    

    fun showDecimal ((a, b)) = 
        let 
            fun powerCalc (b, p, n) = 
                if (Bigint.isequal(Bigint.modulo(b, p), Bigint.zero)) then powerCalc(Bigint.divide(b, p), p, n+1)
                else (b, n)

            fun recHelp (power10, m, divisor) =
                let val rem = Bigint.showInt(Bigint.modulo(Bigint.toInt(power10), divisor))
                in 
                    if (rem = "1") then m
                    else recHelp(rem^"0", m+1, divisor)
                end

            val (without2, power2) = powerCalc(b, Bigint.toInt("2"), 0)
            val (without_2_5, power5) = powerCalc(without2, Bigint.toInt("5"), 0)
            val nonrecl = if(power2>power5) then power2 else power5
            val recl = if(Bigint.isequal(without_2_5, Bigint.unity)) then 1 else recHelp ("10", 1, without_2_5)
            val num = Bigint.showInt(Bigint.divide(Bigint.toInt(addZeroes(Bigint.showInt(a), recl+nonrecl)), b))
            val numsize = size(num)
        
        in  
            if(numsize > recl+nonrecl) then
                if(recl = 0) then substring(num, 0, numsize-nonrecl)^"."^substring(num, numsize-nonrecl, nonrecl)^"(0)"
                else substring(num, 0, numsize-nonrecl-recl)^"."^substring(num, numsize-nonrecl-recl, nonrecl)^"("^substring(num, numsize-recl, recl)^")"
            else if(numsize = recl+nonrecl) then
                if(recl = 0) then "0."^num^"(0)"
                else "0."^substring(num, 0, nonrecl)^"("^substring(num, nonrecl, recl)^")"
            else
                if(recl = 0) then addZeroes("0.", numsize-nonrecl)^num^"(0)"
                else if(numsize<recl) then addZeroes("0.", nonrecl)^"("^addZeroes("", recl-numsize)^num^")"
                else addZeroes("0.", nonrecl+recl-numsize)^substring(num, 0, numsize-recl)^"("^substring(num, numsize-recl, recl)^")"
        end

    fun fromDecimal (intstring) =
        let 
            fun tokenize(numlist, sign, intornotrec, intpart, nonrecpart, recpart) =
                if(numlist = []) then raise rat_error
                else if (numlist = #")"::[]) then (intpart, nonrecpart, recpart, sign)
                else if(hd(numlist) = #"+") then if(intornotrec=0) then tokenize(tl(numlist), false, 0, "", "", "") else raise rat_error
                else if(hd(numlist) = #"~") then if(intornotrec=0) then tokenize(tl(numlist), true, 0, "", "", "") else raise rat_error
                else if(hd(numlist) = #".") then if(intornotrec=0) then tokenize(tl(numlist), sign, 1, intpart, "", "") else raise rat_error
                else if(hd(numlist) = #"(") then if(intornotrec=1) then tokenize(tl(numlist), sign, 2, intpart, nonrecpart, "") else raise rat_error
                else if(Char.ord (hd(numlist)) >= Char.ord #"0" andalso Char.ord (hd(numlist)) <= Char.ord #"9") then   
                    if(intornotrec = 0) then tokenize(tl(numlist), sign, 0, intpart^(Char.toString(hd(numlist))), "", "")
                    else if (intornotrec = 1) then tokenize(tl(numlist), sign, 1, intpart, nonrecpart^(Char.toString(hd(numlist))), "")
                    else if (intornotrec = 2) then tokenize(tl(numlist), sign, 2, intpart, nonrecpart, recpart^(Char.toString(hd(numlist))))
                    else raise rat_error
                else raise rat_error
            
            val (intpart, nonrecpart, recpart, sign) = 
                case tokenize(explode(intstring), false, 0, "", "", "") of
                  ("", nonrecpart, recpart, sign) => ("0", nonrecpart, recpart, sign)
                | (intpart, nonrecpart, recpart, sign) => (intpart, nonrecpart, recpart, sign)

            val abs_rat_o = make_rat(Bigint.sub(Bigint.toInt(intpart^nonrecpart^recpart), Bigint.toInt(intpart^nonrecpart)), 
                            Bigint.sub(Bigint.toInt(addZeroes("1",size(nonrecpart)+size(recpart))), Bigint.toInt(addZeroes("1", size(nonrecpart)))))

        in
            case abs_rat_o of NONE => raise rat_error
            | SOME abs_rat => if(sign = true) then neg(abs_rat) else abs_rat
        end

    fun toDecimal(a,b) = showDecimal(a,b)
end;

structure Rational = createRational(BigInt);