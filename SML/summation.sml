datatype expr = Number of int 
              | Plus of expr * expr
              | Times of expr * expr 
              | Divide of expr * expr 
              | Subtract of expr * expr

fun eval (Number n) = n
 |  eval (Plus (e1, e2)) = eval(e1) + eval(e2)
 |  eval (Times (e1, e2)) = eval(e1) * eval(e2)
 |  eval (Divide (e1, e2)) = eval(e1) div eval(e2)
 |  eval (Subtract (e1, e2)) = eval(e1) - eval(e2);
 
fun evalToExpr (Number n) = Number n
 |  evalToExpr (Plus (e1, e2)) = Number(eval(e1) + eval(e2))
 |  evalToExpr (Times (e1, e2)) = Number(eval(e1) * eval(e2))
 |  evalToExpr (Divide (e1, e2)) = Number(eval(e1) div eval(e2))
 |  evalToExpr (Subtract (e1, e2)) = Number(eval(e1) - eval(e2));
 
fun isCharDigit(chr) =
    if ord(chr) >= 48 andalso ord(chr) <= 57 then true
    else false;

fun parseExpression(L::L2::Ls) =
    let
        fun parSumExpression(L::[]) = parseMulExpression(L::[])
         |  parSumExpression(L::L2::Ls) =
            if ord(L2) = 43 andalso isCharDigit(L) then Plus(parSumExpression(L::[]), parseMulExpression(Ls))
            else if ord(L2) = 45 andalso isCharDigit(L) then Subtract(parSumExpression(L::[]), parseMulExpression(Ls))
            else parseMulExpression(L::L2::Ls)
        
        and parseMulExpression(L::[]) = parseRootExpression (L::[])
         |  parseMulExpression(L::L2::Ls) =
            if ord(L2) = 42 andalso isCharDigit(L) then Times(parseMulExpression(L::[]), parseRootExpression(Ls))
            else if ord(L2) = 47 andalso isCharDigit(L) then Divide(parseMulExpression(L::[]), parseRootExpression(Ls))
            else parseRootExpression(L::L2::Ls)

        and parseRootExpression (L::[]) = Number(ord(L) - 48)
         |  parseRootExpression(L::L2::Ls) =
            parseExpression(L::L2::Ls)
    in
        parSumExpression(L::L2::Ls)
    end

fun stringToIntExpr (S) = 
    eval(parseExpression(explode(S)));

fun replaceValueInCharList (L, n) = map (fn(x) => if x = #"n" then n else x) L;
    
fun summationWithFunction(fntn,n,i) = if i = n then fntn(i) else fntn(i) + summationWithFunction(fntn,n,(i + 1));

fun summationWithString(str, n, i) = 
    if i >= n then stringToIntExpr(implode(replaceValueInCharList(explode(str), chr(i+48)))) 
    else stringToIntExpr(implode(replaceValueInCharList(explode(str), chr(i+48)))) + summationWithString(str, n, (i + 1));

