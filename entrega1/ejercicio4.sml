open List
open Int
open tigerabs

fun maxargs t =   
        case t of
            SubscriptVar (v, e)  => 
                    max ((maxargsExp e), (maxargs v))
            | FieldVar (v, _)     => maxargs v
            | _                   => 0

and 
maxargsExp (VarExp (v, _))  = maxargs v
| maxargsExp (CallExp ({func = "print", args}, _))  = 
    max ((length args), (foldr max 0 (map maxargsExp args)))
| maxargsExp (CallExp ({args, ...}, _)) = 
    foldr max 0 (map maxargsExp args)
| maxargsExp (OpExp ({left,right, ...}, _)) = 
    max ((maxargsExp left), (maxargsExp right))
| maxargsExp (RecordExp ({fields, ...}, _))  = 
    foldr max 0  (map (fn t => maxargsExp (#2 t)) fields)
| maxargsExp (SeqExp (l, _)) = 
    foldr max 0 (map maxargsExp l)
| maxargsExp (AssignExp ({exp, ...}, _)) = 
    maxargsExp exp 
| maxargsExp (IfExp ({test, then', else' = SOME e}, _)) =
    max(max(maxargsExp test, maxargsExp then'),maxargsExp e)
| maxargsExp (IfExp ({test, then', else' = NONE}, _)) =
    max (maxargsExp test, maxargsExp then')
| maxargsExp (WhileExp ({test, body}, _)) =
    max (maxargsExp test, maxargsExp body)
| maxargsExp (ForExp ({lo, hi, body, ...}, _)) =
    max(max(maxargsExp lo, maxargsExp hi), maxargsExp body)
| maxargsExp (LetExp({decs, body}, _)) =
    max(foldr max 0 (map maxargsDec decs), maxargsExp body)
| maxargsExp (ArrayExp ({size, init, ...}, _)) =
    max (maxargsExp size, maxargsExp init)
| maxargsExp _ = 0

and
maxargsDec (FunctionDec []) = 0
| maxargsDec (FunctionDec (({body, ...}, _)::xs)) =
    max(maxargsExp body, maxargsDec (FunctionDec xs))
| maxargsDec (VarDec ({init, ...}, _)) =
    maxargsExp init
| maxargsDec _ = 0
