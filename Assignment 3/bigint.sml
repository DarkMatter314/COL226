signature BIGINT =
sig
    type bigint
    exception bigintError
    val zero : bigint
    val unity : bigint
    val add : bigint * bigint -> bigint
    val sub : bigint * bigint -> bigint
    val mul : bigint * bigint -> bigint
    val divide : bigint * bigint -> bigint 
    val modulo : bigint * bigint -> bigint
    val gcd : bigint * bigint -> bigint
    val abs : bigint -> bigint
    val iszero : bigint -> bool
    val isequal : bigint * bigint -> bool
    val isnegative : bigint -> bool
    val isless : bigint * bigint -> bool
    val negate : bigint -> bigint
    val showInt : bigint -> string
    val toInt : string -> bigint
end;

structure BigInt : BIGINT =
struct
    type bigint = int list * bool;
    exception bigintError;

    val zero = ([], false)
    val unity = ([1], false)

    fun add ((a,s1), ([],s2)) = (a,s1)
    | add (([],s1), (b,s2)) = (b,s2)
    | add ((a,false), (b,true)) = sub((a,false), (b,false))
    | add ((a,true), (b,false)) = sub((b,false), (a,false))
    | add ((a,s1), (b,s2)) = 
        if (s1 <> s2) then raise bigintError else 

        let fun addhelp ([], [], 0) = []
        | addhelp ([], [], carry) = [carry]
        | addhelp (num1, [], carry) =
            let
                val add = hd(num1) + carry;
                val newDigit = add mod 10;
                val newCarry = add div 10;
            in
                newDigit::addhelp(tl(num1), [], newCarry)
            end
        | addhelp ([], num2, carry) = addhelp(num2, [], carry)
        | addhelp (num1, num2, carry) =
            let
                val add = hd(num1) + hd(num2) + carry;
                val newDigit = add mod 10;
                val newCarry = add div 10;
            in
                newDigit::addhelp(tl(num1), tl(num2), newCarry)
            end

        in (addhelp (a, b, 0),s1)
        end

    and sub ((a,s1), ([],s2)) = (a,s1)
    | sub (([],s1), (b,s2)) = (b,(not s2))
    | sub ((a,false), (b,true)) = add ((a,false), (b,false))
    | sub ((a,true), (b,false)) = add ((a,true), (b,true))
    | sub ((a,true), (b,true)) = sub ((b,false), (a,false))
    | sub ((a,false), (b,false)) = 

    let
        fun subCheck (x::restx, [], check) = true
        | subCheck ([], y::resty, check) = false
        | subCheck (x::[], y::[], check) = if (x>y) then true else if (x=y) then check else false
        | subCheck (x, y, check) = if (hd(x)>hd(y)) then subCheck (tl(x), tl(y), true) else if (hd(x)=hd(y)) then subCheck (tl(x), tl(y), check) else subCheck (tl(x), tl(y), false)

        fun subhelp ([], [], 0) = []
        | subhelp (num1, [], borrow) = 
            let
                val diff = hd(num1) - borrow;
                val newBorrow = if diff<0 then 1 else 0;
                val newDigit = if diff<0 then diff+10 else diff;
                val newList = subhelp (tl(num1), [], newBorrow)
            in
                if (newList = []) then
                    if(newDigit = 0) then []
                    else [newDigit]
                else newDigit :: (newList)
            end
        | subhelp (num1, num2, borrow) = 
            let
                val diff = hd(num1) - hd(num2) - borrow;
                val newBorrow = if diff<0 then 1 else 0;
                val newDigit = if diff<0 then diff+10 else diff;
                val newList = subhelp (tl(num1), tl(num2), newBorrow)
            in
                if (newList = []) then
                    if(newDigit = 0) then []
                    else [newDigit]
                else newDigit :: (newList)
            end;
        
        val ch = subCheck (a, b, true)

    in if (ch) then (subhelp(a,b,0),false) else (subhelp(b,a,0),true)
    end;

    fun mul (a, ([],s2)) = ([], false)
    | mul (([], s1), b) = ([], false)
    | mul ((a, s1), (b,s2)) =
    
    let 
        fun mulDigit (number , digit, carry) =
            if (digit = 0) then []
            else if (number = []) then
                if (carry = 0) then [] else [carry]
            else
                let
                    val prod = hd(number)*digit + carry;
                    val newCarry = prod div 10;
                    val newDigit = prod mod 10;
                in 
                    newDigit :: mulDigit (tl(number), digit, newCarry)
                end;
        
        val thismul = mulDigit (a, hd(b), 0);
        val (nextmul, finalsign) = mul ((a, s1), (tl(b), s2));
    
    in (#1 (add((thismul,false), (0::nextmul, false))), s1 orelse s2)
    end;

    fun remove_trailing_zeroes ([]) = []
    | remove_trailing_zeroes (num) = 
        let val after = remove_trailing_zeroes (tl(num))
        in if(hd(num) = 0 andalso after = []) then []
           else hd(num)::after
        end;
    
    fun remove_leading_zeroes(lst: int list) =
        case lst of [] => []
        | 0::rest => remove_leading_zeroes rest
        | _ => lst


    fun divhelp (numList, dividend, divisor, quotient) =
        let 
            fun checkMul (numList, divisor, quotient, k) = 
                if (k=0) then (numList, k::quotient)
                else if (k>9 orelse k<0) then raise bigintError
                else    
                    let val (remainder, sign) = sub((numList, false), (mul((divisor, false), ([k], false))))
                    in if (sign = false) then (remainder, k::quotient)
                       else checkMul (numList, divisor, quotient, k-1)
                    end
            
            val newList = hd(dividend) :: numList
            val (newDividend, newQuotient) = checkMul (newList, divisor, quotient, 9)
            val newDividend_wo_0 = remove_trailing_zeroes(newDividend)
            val newQuotient_wo_0 = remove_trailing_zeroes(newQuotient)
        in  
            if(tl(dividend) = []) then (newQuotient_wo_0, newDividend_wo_0)
            else divhelp (newDividend_wo_0, tl(dividend), divisor, newQuotient_wo_0)
        end

    fun divide (num1, ([], s2)) = raise bigintError
    | divide (num1, ([0], s2)) = raise bigintError
    | divide (([], s1), num2) = ([], false)
    | divide (([0], s1), num2) = ([], false)
    | divide ((num1, s1), (num2, s2)) =
        let
            val (quotient, remainder) = divhelp([], rev(num1), num2, []);
            val quotient_wo_0 = remove_trailing_zeroes(quotient)
            val remainder_wo_0 = remove_trailing_zeroes(remainder)
        in (quotient_wo_0, ((s1 andalso not s2) orelse (not s1 andalso s2)))
        end

    fun modulo (num1, ([], s2)) = raise bigintError
    | modulo (num1, ([0], s2)) = raise bigintError
    | modulo (([], s1), num2) = ([], false)
    | modulo (([0], s1), num2) = ([], false)
    | modulo ((num1, s1), (num2, s2)) = 
        let
            val (quotient, remainder) = divhelp([], rev(num1), num2, []);
            val quotient_wo_0 = remove_trailing_zeroes(quotient)
            val remainder_wo_0 = remove_trailing_zeroes(remainder)
        in if (remainder = [] orelse remainder = [0]) then ([], false) else (remainder_wo_0, s1)
        end

    fun gcd ((a,s1), ([],s2)) = (a, s1)
    | gcd (([],s1), (b,s2)) = (b, s2)
    | gcd ((a,s1), (b,s2)) = gcd((b,s2), modulo((a,s1), (b,s2)))

    fun abs ((a, s)) = (a, false)

    fun iszero (([], s1)) = true
    | iszero (([0], s1)) = true
    | iszero ((a,s1)) = false

    fun isequal (a, b) = iszero(sub(a,b))

    fun isnegative ((a, s)) =
        if(a = [] orelse a=[0]) then false
        else s

    fun isless (a,b) = isnegative(sub(a,b))

    fun negate (([],s)) = ([], s)
    | negate ([0], s) = ([], s)
    | negate (a, s) = (a, not s)

    fun showInt (([], s)) = "0"
    | showInt (([0], s)) = "0"
    | showInt((a, true)) = "~"^showInt((a,false))
    | showInt((a, false)) = String.concat(List.map Int.toString (List.rev (remove_trailing_zeroes(a))))

    fun toInt ("0") = ([], false)
    | toInt ("") = ([], false)
    | toInt (intstring) =
        if(substring(intstring, 0, 1) = "~") then (List.rev (remove_leading_zeroes(List.map (fn x => Char.ord x - Char.ord #"0")(explode(substring(intstring, 1, size intstring - 1))))), true)
        else if(substring(intstring, 0, 1) = "+") then (List.rev (remove_leading_zeroes(List.map (fn x => Char.ord x - Char.ord #"0")(explode(substring(intstring, 1, size intstring - 1))))), true)
        else (List.rev (remove_leading_zeroes(List.map (fn x => Char.ord x - Char.ord #"0")(explode(intstring)))), false)

end;