datatype expr = Number of int 
              | Plus of expr * expr
              | Times of expr * expr 
              | Divide of expr * expr 
              | Minus of expr * expr

datatype token = NUMBER of int
               | PLUS
               | TIMES
               | DIVIDE
               | MINUS
              
fun eval (Number n) = n
 |  eval (Plus (e1, e2)) = eval(e1) + eval(e2)
 |  eval (Times (e1, e2)) = eval(e1) * eval(e2)
 |  eval (Divide (e1, e2)) = eval(e1) div eval(e2)
 |  eval (Minus (e1, e2)) = eval(e1) - eval(e2);
 
fun evalToExpr (Number n) = Number n
 |  evalToExpr (Plus (e1, e2)) = Number(eval(e1) + eval(e2))
 |  evalToExpr (Times (e1, e2)) = Number(eval(e1) * eval(e2))
 |  evalToExpr (Divide (e1, e2)) = Number(eval(e1) div eval(e2))
 |  evalToExpr (Minus (e1, e2)) = Number(eval(e1) - eval(e2));
 
fun tokenToExpr(NUMBER n) = Number n

fun isCharDigit(chr) =
    if ord(chr) >= 48 andalso ord(chr) <= 57 then true
    else false;

fun lexer nil = nil
 |  lexer (#"+"::Ls) = PLUS::lexer(Ls)
 |  lexer (#"*"::Ls) = TIMES::lexer(Ls)
 |  lexer (#"/"::Ls) = DIVIDE::lexer(Ls)
 |  lexer (#"-"::Ls) = MINUS::lexer(Ls)
 |  lexer (L::Ls) = getDigit(0, L::Ls, 1)
and getDigit (p, Ls, n) = 
    if null(Ls) orelse not (isCharDigit(hd Ls)) then NUMBER (p*n)::lexer(Ls)
    else getDigit (p * 10 + ord(hd(Ls)) - ord(#"0") , tl(Ls), n);

fun parseExpression(L::L2::Ls) =
    let
        fun parSumExpression(L::[]) = parseMulExpression(L::[])
         |  parSumExpression(L::L2::Ls) =
            if L2 = PLUS then Plus(parSumExpression(L::[]), parseMulExpression(Ls))
            else if L2 = MINUS then Minus(parSumExpression(L::[]), parseMulExpression(Ls))
            else parseMulExpression(L::L2::Ls)
        
        and parseMulExpression(L::[]) = parseRootExpression (L::[])
         |  parseMulExpression(L::L2::Ls) =
            if L2 = TIMES then Times(parseMulExpression(L::[]), parseRootExpression(Ls))
            else if L2 = DIVIDE then Divide(parseMulExpression(L::[]), parseRootExpression(Ls))
            else parseRootExpression(L::L2::Ls)

        and parseRootExpression (L::[]) = tokenToExpr(L)
         |  parseRootExpression(L::L2::Ls) =
            parseExpression(L::L2::Ls)
    in
        parSumExpression(L::L2::Ls)
    end

fun stringToIntExpr (S) = 
    eval(parseExpression(lexer(explode(S))));

fun replaceValueInCharList (L, n) = map (fn(x) => if x = #"n" then n else x) L;
    
fun summationExample1 x = (x * (x + 1)) div 2;
fun summationExample2 x = (x * (x + 1) * ((2 * x) + 1)) div 6;

fun summationWithFunction(fntn,i,n) = if i = n then fntn(i) else if i > n then 0 else fntn(i) + summationWithFunction(fntn,(i + 1),n);

fun summationWithString(str,i,n) = 
    if i >= n then stringToIntExpr(implode(replaceValueInCharList(explode(str), chr(i+48)))) 
    else stringToIntExpr(implode(replaceValueInCharList(explode(str), chr(i+48)))) + summationWithString(str, (i + 1), n);

