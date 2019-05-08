datatype expr = Number of int 
              | Plus of expr * expr
              | Times of expr * expr 
              | Divide of expr * expr 
              | Subtract of expr * expr
              | (expr)

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
 
fun charListToExpr (C::[]) = Number(ord(C) - 48)
 |  charListToExpr (C::C2::Cs) =
    if ord(C2) = 43 then Plus(Number(ord(C) - 48), charListToExpr(Cs))
    else if ord(C2) = 42 then Times(Number(ord(C) - 48), charListToExpr(Cs))
    else if ord(C2) = 47 then Divide(Number(ord(C) - 48), charListToExpr(Cs))
    else if ord(C2) = 45 then Subtract(Number(ord(C) - 48), charListToExpr(Cs))
    else Number(ord(C) - 48)
    
fun parseExpression(L::L2::Ls) =
    let
        fun parSumExpression(L::L2::Ls) =
            if ord(L2) = 43 then Plus(Number(ord(L) - 48), parseMulExpression(Ls))
            else if ord(L2) = 45 then Subtract(Number(ord(L) - 48), parseMulExpression(Ls))
            else parseMulExpression(L::L2::Ls)
        
        and parseMulExpression(L::[]) = parseRootExpression (L::[])
         |  parseMulExpression(L::L2::Ls) =
            if ord(L2) = 42 then Times(Number(ord(L) - 48), parseRootExpression(Ls))
            else if ord(L2) = 47 then Divide(Number(ord(L) - 48), parseRootExpression(Ls))
            else parseRootExpression(L::L2::Ls)

        and parseRootExpression (L::[]) = Number(ord(L) - 48)
         |  parseRootExpression(L::L2::Ls) =
            parseExpression(L::L2::Ls)
    in
        parSumExpression(L::L2::Ls)
    end

fun stringToIntExpr (S) = 
    eval(parseExpression(explode(S)));


