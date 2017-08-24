open List
open Int
open tigerabs

fun maxargs t =   
        case t of
            SubscriptVar (v, e)  => max ((maxargsExp e), (maxargs v))
            | FieldVar (v, _)     => maxargs v
            | _                   => 0

and 
maxargsExp (VarExp (v, _))  = maxargs v
| maxargsExp (CallExp ({func = "print", args}, _))  = max ((length args), (foldr max (map maxargsExp args)) )
| maxargsExp (CallExp ({args, ...}, _)) = foldr 0  max (map maxargsExp args)
| maxargsExp (OpExp ({l,r, ...}, _)) = max ((maxargsExp l), (maxargsExp r))
| maxargsExp (RecordExp ({f, ...}, _)) = foldr 0  max (map (fn t => maxargsExp #2(t)) f)
| maxargsExp _ = 0
