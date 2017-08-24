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
| maxargsExp (CallExp ({func = "print", args}, _))  = max ((length args), (foldr max 0 (map maxargsExp args)) )
| maxargsExp (CallExp ({args, ...}, _)) = foldr max 0 (map maxargsExp args)
| maxargsExp (OpExp ({left,right, ...}, _))    = max ((maxargsExp left), (maxargsExp right))
| maxargsExp (RecordExp ({fields, ...}, _))  = foldr max 0  (map (fn t => maxargsExp (#2 t)) fields)
| maxargsExp _ = 0
