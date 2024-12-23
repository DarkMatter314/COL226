(* Multilies number represented as list by a single digit *)
fun multiplyList (number , digit, carry) =
    if (digit = 0) then []
    else if (number = []) then
        if (carry = 0) then [] else [carry]
    else
        let
            val prod = hd(number)*digit + carry;
            val newCarry = prod div 10;
            val newDigit = prod mod 10;
        in 
            newDigit :: multiplyList (tl(number), digit, newCarry)
        end;

(* Subtracts two numbers represented as lists, returns -1 if subtraction is negative *)
fun subtractLists (num1, num2, carry) =
    if(num1 = [] andalso num2 = []) then
        if carry = 0 then [] else [~1]
    else if (num1 = []) then [~1]
    else if (num2 = []) then
        let
            val diff = hd(num1) - carry;
            val newCarry = if diff<0 then 1 else 0;
            val newDigit = if diff<0 then diff+10 else diff;
            val newList = subtractLists (tl(num1), num2, newCarry)
        in
            if (newList = []) then
                if(newDigit = 0) then []
                else [newDigit]
            else if (hd(newList) = ~1) then [~1]
            else newDigit :: (newList)
        end
    else 
        let
            val diff = hd(num1) - hd(num2) - carry;
            val newCarry = if diff<0 then 1 else 0;
            val newDigit = if diff<0 then diff+10 else diff;
            val newList = subtractLists (tl(num1), tl(num2), newCarry)
        in
            if (newList = []) then
                if(newDigit = 0) then []
                else [newDigit]
            else if (hd(newList) = ~1) then [~1]
            else newDigit :: (newList)
        end;

(* Adds two numbers, represented as lists *)
fun addLists (num1, num2, carry) = 
    if(num1 = [] andalso num2 = []) then
        if (carry = 0) then [] else [carry]
    else if (num1 = []) then
        let
            val add = hd(num2) + carry;
            val newDigit = add mod 10;
            val newCarry = add div 10;
        in
            newDigit::addLists(num1, tl(num2), newCarry)
        end
    else if (num2 = []) then
        let
            val add = hd(num1) + carry;
            val newDigit = add mod 10;
            val newCarry = add div 10;
        in
            newDigit::addLists(tl(num1), num2, newCarry)
        end
    else
        let
            val add = hd(num1) + hd(num2) + carry;
            val newDigit = add mod 10;
            val newCarry = add div 10;
        in
            newDigit::addLists(tl(num1), tl(num2), newCarry)
        end;

(* Finds the next digit in the square root *)
fun checkMultiply(numlist, divisor, quotient, k) =
    if(k = 10) then (subtractLists(numlist, multiplyList((k-1)::(divisor), k-1, 0), 0), addLists((k-1)::(divisor), [k-1], 0), (k-1)::(quotient))
    else
        let
            val sub = subtractLists(numlist, multiplyList(k::(divisor), k, 0), 0);
        in
            if(sub = []) then
                (sub, addLists(k::(divisor), [k], 0), k::(quotient))
            else if(hd(sub) = ~1) then
                (subtractLists(numlist, multiplyList((k-1)::(divisor), k-1, 0), 0), addLists((k-1)::(divisor), [k-1], 0), (k-1)::(quotient))
            else checkMultiply(numlist, divisor, quotient, k+1)
        end;

(* Recursion Function *)
fun isqrtld_rec (numList, dividend, divisor, quotient) = 
    let
        val newList = hd(tl(numList)) :: hd(numList) :: (dividend);
        val (newDividend , newDivisor, newQuotient) = checkMultiply(newList, divisor, quotient, 1);
    in
        if(tl(tl(numList)) = []) then (newQuotient, newDividend)
        else isqrtld_rec(tl(tl(numList)), newDividend, newDivisor, newQuotient)
    end;

fun isqrtld(number) = 
    let
        val intList = List.map (fn x => Char.ord x - Char.ord #"0")(explode(number));
        val numList = if(length(intList) mod 2 = 0) then intList else 0::(intList);
        val (sqroot, remainder) = isqrtld_rec (numList, [], [], []);
    in
        if(remainder = []) then (String.concat(List.map Int.toString (List.rev sqroot)),"0")
        else (String.concat(List.map Int.toString (List.rev sqroot)),String.concat(List.map Int.toString (List.rev remainder)))
    end;